import os, sys, time
import numpy as np
import pandas as pd
import geopandas as gpd
import sqlalchemy
import pyodbc
from shapely import wkt
from scipy.spatial import cKDTree
from shapely.geometry import Point

def read_from_sde(connection_string, feature_class_name, version,
                  crs='EPSG:2285', cols=None):

    """
    Returns the specified feature class as a geodataframe from ElmerGeo.
    
    Parameters
    ----------
    connection_string : SQL connection string that is read by geopandas 
                        read_sql function
    
    feature_class_name: the name of the featureclass in PSRC's ElmerGeo 
                        Geodatabase
    
    cs: cordinate system
    """
    engine = sqlalchemy.create_engine(connection_string)
    con=engine.connect()
    df = pd.read_sql('select *, Shape.STAsText() as geometry from %s' % 
                (feature_class_name), con=con)

    df['geometry'] = df['geometry'].apply(wkt.loads)
    gdf = gpd.GeoDataFrame(df, geometry='geometry')
    gdf.crs = crs
    if not cols:
        cols = [col for col in gdf.columns if col not in 
            ['Shape', 'GDB_GEOMATTR_DATA', 'SDE_STATE_ID']]
    gdf = gdf[cols]
    
    return gdf

def find_nearest(gdA, gdB):
    """ Find nearest value between two geodataframes.
        Returns "dist" for distance between nearest points.
    """

    nA = np.array(list(gdA.geometry.apply(lambda x: (x.x, x.y))))
    nB = np.array(list(gdB.geometry.apply(lambda x: (x.x, x.y))))
    btree = cKDTree(nB)
    dist, idx = btree.query(nA, k=1)
    gdB_nearest = gdB.iloc[idx].drop(columns="geometry").reset_index(drop=True)
    gdf = pd.concat(
        [
            gdA.reset_index(drop=True),
            gdB_nearest,
            pd.Series(dist, name='dist')
        ], 
        axis=1)

    return gdf

def weighted_avg(df, val_col, wt_col, agg_col):
    """ Returns weighted average for specified aggregation. 
        
        Parameters
    ----------
    df : Pandas DataFrame 
    val_col: column name of the value being averaged
    wt_col: weight column name
    agg_col: column to be used for aggregation
    ----------
    """

    df['wt_tot'] = df[val_col]*df[wt_col]
    df_agg = df.groupby(agg_col).sum()
    df_agg['wt_avg'] = df_agg['wt_tot']/df_agg[wt_col]
    
    return df_agg[['wt_avg']]

#######################################################
# Calculate average weighted distance to features
#######################################################

def main():
     
    # Define SQL connections for ElmerGeo
    elmergeo_conn_string = 'mssql+pyodbc://AWS-PROD-SQL\Sockeye/ElmerGeo?driver=SQL Server?Trusted_Connection=yes'
    crs = 'EPSG:2285'
    version = "'DBO.Default'"

    # Connections to Elmer
    elmer_conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
    sql_conn = pyodbc.connect(elmer_conn_string)

    # Load parcel data
    print('Loading parcel points...')
    parcels_gdf = read_from_sde(elmergeo_conn_string, 'parcels_urbansim_2018_pts', version, crs=crs)
    parcels_gdf = parcels_gdf[['parcel_id','geometry']]

    # Load HCT location data
    print('Loading HCT geospatial data...')
    hct_gdf = read_from_sde(elmergeo_conn_string, 'hct_station_areas', version, crs=crs)
    hct_gdf.geometry = hct_gdf.centroid

    # Load 2020 Census block groups
    # FIXME: if layers don't load, wait and try again
    print('Loading Census data...')
    block_grp_gdf = read_from_sde(elmergeo_conn_string, 'blockgrp2020', version, crs=crs)
    block_grp_gdf = block_grp_gdf[['geoid20','geometry']]
    print('Overlaying parcels on Census data...')
    parcels_gdf = gpd.overlay(parcels_gdf, block_grp_gdf)

    # Use households per parcel as a weight
    # FIXME: use Elmer when data is available
    #R:\e2projects_two\SoundCast\Inputs\dev\landuse\2018\v3_RTP
    print('Loading parcel household data...')
    df_parcel_hh = pd.read_csv(r'Y:\Equity Indicators\access\parcels_urbansim.txt', 
                               sep=' ', usecols=['parcelid','hh_p'])
    parcels_gdf = parcels_gdf.merge(df_parcel_hh, left_on='parcel_id', right_on='parcelid')

    # Load equity quintiles definitions and join to block groups
    # # FIXME: run directly from the R Script? 
    equity_shares_df = pd.read_csv(r'Y:\Equity Indicators\access\equity_shares.csv')
    parcels_gdf['GEOID'] = parcels_gdf['geoid20'].astype('int64')
    parcels_gdf = parcels_gdf.merge(equity_shares_df, on='GEOID', how='left')

    # Drop any null rows
    print("Removed %s null of %d parcels" %(len(parcels_gdf[parcels_gdf['Older_quintile'].isnull()]),len(parcels_gdf)))
    parcels_gdf = parcels_gdf[~parcels_gdf['Older_quintile'].isnull()]

    #######################################################
    # Access to Transit Stations
    #######################################################

    print('Calculating weighted distance...')

    # Iterate through submodes
    submode_dict = {'all_hct': hct_gdf,
                    'light_rail': hct_gdf[hct_gdf['light_rail'] != 0],
                    'commuter_rail': hct_gdf[hct_gdf['commuter_r'] != 0],
                    'ferry': hct_gdf[hct_gdf['ferry'] != 0],
                    'passenger_ferry': hct_gdf[hct_gdf['passenger_'] != 0],
                    'brt' : hct_gdf[hct_gdf['brt'] != 0]}

    results_dict = {}
    for submode, df in submode_dict.items():
        results_dict[submode] = find_nearest(parcels_gdf, df)
        results_dict[submode]['miles'] = results_dict[submode]['dist']/5280.0

    # Iterate over measures and submodes
    full_output_df = pd.DataFrame()
    for agg_col in ['POC_quintile', 'Income_quintile', 'Disability_quintile',
                    'Youth_quintile', 'Older_quintile', 'LEP_quintile']:
        for submode in ['all_hct','light_rail','commuter_rail','ferry','passenger_ferry','brt']:
            df = weighted_avg(results_dict[submode], 'miles', 'hh_p', agg_col)
            df = df.reset_index()
            df.rename(columns={agg_col: 'quintile'}, inplace=True)
            df['access_to'] = submode
            df['aggregation'] = agg_col.split('_')[0]
            full_output_df = pd.concat([full_output_df,df.reset_index()])

    # add label for qunitle
    quintile_def = {
        1: 'Low',
        2: 'Low Medium',
        3: 'Medium',
        4: 'Medium High',
        5: 'High'}
    full_output_df['quintile'] = full_output_df['quintile'].astype('int').map(quintile_def)

    #add a label to wt_avg_miles
    full_output_df.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)

    # Write to local dir
    full_output_df.to_csv(r'distance_indicator.csv', index=False)

if __name__ == '__main__':
    start_time = time.time()
    main()
    print("--- %s minutes ---" % ((time.time() - start_time)/60.0))