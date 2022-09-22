import os, sys, time
import numpy as np
import pandas as pd
import geopandas as gpd
import sqlalchemy
from shapely import wkt
from scipy.spatial import cKDTree
from shapely.geometry import Point
from sqlalchemy.engine import URL
from pymssql import connect
import warnings
warnings.filterwarnings('ignore')

# Define which year to use for equity quintile definitions (from Elmer)
# As of Sept 2022, options are 2019, 2020
equity_quintiles_year = 2020

def load_elmer_geo_table(feature_class_name, con, crs):
    """ Load ElmerGeo table as geoDataFrame, applying a specified coordinate reference system (CRS)
    """
    geo_col_stmt = "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=" + "\'" + feature_class_name + "\'" + " AND DATA_TYPE='geometry'"
    geo_col = str(pd.read_sql(geo_col_stmt, con).iloc[0,0])
    query_string = 'SELECT *,' + geo_col + '.STGeometryN(1).ToString()' + ' FROM ' + feature_class_name

    df = pd.read_sql(query_string, con)
    df.rename(columns={'':'geometry'}, inplace = True)
    df['geometry'] = df['geometry'].apply(wkt.loads)

    gdf = gpd.GeoDataFrame(df, geometry='geometry')
    gdf.crs = crs

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
    crs = 'EPSG:2285'
    elmergeo_conn_string = 'AWS-Prod-SQL\Sockeye'
    elmergeo_con = connect('AWS-Prod-SQL\Sockeye', database="ElmerGeo")

    # Connections to Elmer
    elmer_conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": elmer_conn_string})
    elmer_engine = sqlalchemy.create_engine(connection_url)

    # Load equity quintiles definitions and join to block groups
    equity_shares_df = pd.read_sql(sql='select * from equity.v_blockgroup_shares', con=elmer_engine)
    equity_shares_df = equity_shares_df[equity_shares_df['data_year'] == equity_quintiles_year]
    #equity_shares_df = pd.read_csv(r'Y:\Equity Indicators\access\equity_shares.csv')

    # Load parcel data
    print('Loading parcel points...')
    parcels_gdf = load_elmer_geo_table('parcels_urbansim_2018_pts', elmergeo_con, crs)
    #parcels_gdf = read_from_sde(elmergeo_conn_string, 'parcels_urbansim_2018_pts', version, crs=crs)
    parcels_gdf = parcels_gdf[['parcel_id','geometry']]

    # Load HCT location data
    print('Loading HCT geospatial data...')
    hct_gdf = load_elmer_geo_table('hct_station_areas', elmergeo_con, crs)
    hct_gdf.geometry = hct_gdf.centroid

    # Load 2020 Census block groups
    # FIXME: if layers don't load, wait and try again
    print('Loading Census data...')
    block_grp_gdf = load_elmer_geo_table('blockgrp2020', elmergeo_con, crs)
    block_grp_gdf = block_grp_gdf[['geoid20','geometry']]
    print('Overlaying parcels on Census data...')
    parcels_gdf = gpd.overlay(parcels_gdf, block_grp_gdf)

    # Use households per parcel as a weight
    # FIXME: use Elmer when data is available
    print('Loading parcel household data...')
    df_parcel_hh = pd.read_csv(r'Y:\Equity Indicators\access\parcels_urbansim.txt', 
                               sep=' ', usecols=['parcelid','hh_p'])
    parcels_gdf = parcels_gdf.merge(df_parcel_hh, left_on='parcel_id', right_on='parcelid')

    # Merge equity share geographies
    parcels_gdf['geoid'] = parcels_gdf['geoid20'].astype('int64')
    equity_shares_df['geoid'] = equity_shares_df['geoid'].astype('int64')
    parcels_gdf = parcels_gdf.merge(equity_shares_df, on='geoid', how='left')

    # Drop any null rows
    print("Removed %s null of %d parcels" %(len(parcels_gdf[parcels_gdf['older_quintile'].isnull()]),len(parcels_gdf)))
    parcels_gdf = parcels_gdf[~parcels_gdf['older_quintile'].isnull()]

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
    for agg_col in ['poc_quintile', 'income_quintile', 'disability_quintile',
                    'youth_quintile', 'older_quintile', 'lep_quintile']:
        for submode in ['all_hct','light_rail','commuter_rail','ferry','passenger_ferry','brt']:
            df = weighted_avg(results_dict[submode], 'miles', 'hh_p', agg_col)
            df = df.reset_index()
            df.rename(columns={agg_col: 'quintile'}, inplace=True)
            df['access_to'] = submode
            df['aggregation'] = agg_col.split('_')[0]
            full_output_df = pd.concat([full_output_df,df.reset_index()])

    #add a label to wt_avg_miles
    full_output_df.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)

    # Write to local dir
    full_output_df[['aggregation','quintile','access_to','wt_avg_miles']].to_csv(r'distance_indicator.csv', index=False)

if __name__ == '__main__':
    start_time = time.time()
    main()
    print("--- %s minutes ---" % ((time.time() - start_time)/60.0))