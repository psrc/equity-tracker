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

# Calculate household population within buffered area
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

def intersect(gdf_a, gdf_b, gdf_b_col):
    """ Intersect points from gdf_b with polygon of gdf_a
        Return df of gdf_b within polygon gdf_a.
        *gdf_b_col* is column name for unique point identifier 
    """
    
    gdf_intersect = gpd.overlay(gdf_a, gdf_b, how="intersection", keep_geom_type=False)
    df = gdf_b[gdf_b[gdf_b_col].isin(gdf_intersect[gdf_b_col].unique())]

    return df

#######################################################
# Calculate average weighted distance to features
#######################################################

def main():
     
    # Define SQL connections for ElmerGeo
    crs = 'EPSG:2285'    # NAD83 / Washington North (ftUS)
    elmergeo_conn_string = 'AWS-Prod-SQL\Sockeye'
    elmergeo_con = connect('AWS-Prod-SQL\Sockeye', database="ElmerGeo")

    # Connections to Elmer
    elmer_conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": elmer_conn_string})
    elmer_engine = sqlalchemy.create_engine(connection_url)

    # Load equity quintiles definitions and join to block groups
    equity_shares_df = pd.read_sql(sql='select * from equity.v_blockgroup_shares', con=elmer_engine)
    equity_shares_df = equity_shares_df[equity_shares_df['data_year'] == equity_quintiles_year]

    # Load parcel data
    print('Loading parcel points...')
    parcels_gdf = load_elmer_geo_table('parcels_urbansim_2018_pts', elmergeo_con, crs)
    parcels_gdf = parcels_gdf[['parcel_id','geometry']]

    # Elmer table provides lookup between parcel ID and block group
    # FIXME: use this a table lookup instead of using geographic joins below
    parcel_lookup_df = pd.read_sql(sql='select parcel_id, block_group_geoid10 from small_areas.parcel_dim', con=elmer_engine)

    #test = parcels_gdf.merge(parcel_lookup, on='parcel_id')

    # Load High Capacity Transit (HCT) Coverage Layer (polygon)
    # We are using the Vision 2050 definitions: 
    # 1/4 mi. for BRT, 1/2 mi. for light rail/ferry/commuter rail
    # Excluding rural areas
    print('Loading HCT geospatial data...')
    hct_gdf = load_elmer_geo_table('hct_station_areas', elmergeo_con, crs)

    # Load 2020 Census block groups
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
    # Calculate household population within given proximities
    #######################################################

    # Iterate through submodes
    submode_dict = {'all_hct': hct_gdf,
                    'light_rail': hct_gdf[hct_gdf['light_rail'] != 0],
                    'commuter_rail': hct_gdf[hct_gdf['commuter_r'] != 0],
                    'ferry': hct_gdf[hct_gdf['ferry'] != 0],
                    'passenger_ferry': hct_gdf[hct_gdf['passenger_'] != 0],
                    'brt' : hct_gdf[hct_gdf['brt'] != 0]}

    results_dict = {}

    # Intersect parcel-level households with HCT coverage 
    for submode, df in submode_dict.items():
        results_dict[submode] = intersect(df, parcels_gdf, gdf_b_col='parcelid')

    # Aggregate results by Equity Geography
    # Iterate over measures and submodes
    results_df = pd.DataFrame()
    for agg_col in ['poc_quintile', 'income_quintile', 'disability_quintile',
                    'youth_quintile', 'older_quintile', 'lep_quintile']:
        for submode, df in results_dict.items():
            # Aggregate parcels within HCT area
            _df = df.groupby(agg_col).sum()[['hh_p']].reset_index()
            _df.rename(columns={agg_col: 'quintile', 'hh_p': 'households_in_buffer'}, inplace=True)
            # Aggregate all households and merge with HCT aggregation
            _df_full = parcels_gdf.groupby(agg_col).sum()[['hh_p']].reset_index()
            _df_full.drop(agg_col, axis=1, inplace=True)
            _df_full.rename(columns={'hh_p': 'total_households'}, inplace=True)

            _df = _df.merge(_df_full, left_index=True, right_index=True)
            _df['aggregation'] = agg_col.split('_')[0]
            _df['mode'] = submode
            results_df = pd.concat([results_df,_df.reset_index()])

    # Create a column of shares within HCT aera
    results_df['household_shares_in_buffer'] = results_df['households_in_buffer']/results_df['total_households']

    # Write to local dir
    results_df[['aggregation','quintile','mode','households_in_buffer','total_households','household_shares_in_buffer']].to_csv(r'hct_proximity_households.csv', index=False)

if __name__ == '__main__':
    start_time = time.time()
    main()
    print("--- %s minutes ---" % ((time.time() - start_time)/60.0))