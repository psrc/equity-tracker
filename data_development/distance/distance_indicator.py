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

def aggregate_results(results_dict, geog_level):

    # Iterate over measures and submodes
    full_output_df = pd.DataFrame()
    block_group_avg_df = pd.DataFrame()
    for agg_col in ['poc', 'income', 'disability','youth', 'older', 'lep']:
        for submode in ['all_hct','light_rail','commuter_rail','ferry','passenger_ferry','brt']:
            # Calculate weighted average across all households by equity geographies
            df = weighted_avg(results_dict[submode], 'miles', 'hh_p', agg_col + '_quintile')
            df = df.reset_index()
            df.rename(columns={agg_col + '_quintile': 'quintile'}, inplace=True)
            df['access_to'] = submode
            df['aggregation'] = agg_col
            full_output_df = pd.concat([full_output_df,df.reset_index()])

            # Calculate a weighted average for each block group, weighting by number of people in the equity definition
            df = weighted_avg(results_dict[submode], 'miles', 'hh_p', geog_level)
            df = df.reset_index()
            df['access_to'] = submode
            df['aggregation'] = agg_col
            block_group_avg_df = pd.concat([block_group_avg_df,df.reset_index()])

            return full_output_df, block_group_avg_df

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

    # Load equity quintiles definitions and join to block groups/tracts
    equity_shares_blockgroup_df = pd.read_sql(sql='select * from equity.v_blockgroup_shares', con=elmer_engine)
    equity_shares_blockgroup_df = equity_shares_blockgroup_df[equity_shares_blockgroup_df['data_year'] == equity_quintiles_year]
    equity_shares_tract_df = pd.read_sql(sql='select * from equity.v_tract_shares', con=elmer_engine)
    equity_shares_tract_df = equity_shares_tract_df[equity_shares_tract_df['data_year'] == equity_quintiles_year]

    # Load parcel data
    print('Loading parcel points...')
    parcels_gdf = load_elmer_geo_table('parcels_urbansim_2018_pts', elmergeo_con, crs)
    #parcels_gdf = read_from_sde(elmergeo_conn_string, 'parcels_urbansim_2018_pts', version, crs=crs)
    parcels_gdf = parcels_gdf[['parcel_id','geometry']]

    # Load HCT location data
    print('Loading HCT geospatial data...')
    hct_gdf = load_elmer_geo_table('hct_stops', elmergeo_con, crs)
    hct_gdf.geometry = hct_gdf.centroid   # Should already be a point, but take centroid just in case

    # Load count of people within each equity definition for all block groups and tracts
    equity_counts_blockgroup_df = pd.read_sql(sql='select * from equity.v_blockgroup_counts WHERE data_year=2020', con=elmer_engine)
    equity_counts_tract_df = pd.read_sql(sql='select * from equity.v_tract_counts WHERE data_year=2020', con=elmer_engine)


    # Load 2020 Census block groups
    # FIXME: if layers don't load, wait and try again
    print('Loading Census data...')
    block_grp_gdf = load_elmer_geo_table('blockgrp2020', elmergeo_con, crs)
    block_grp_gdf = block_grp_gdf[['geoid20','tractce20','blkgrpce20','geometry']]

    print('Overlaying parcels on Census data...')
    parcels_gdf = gpd.overlay(parcels_gdf, block_grp_gdf)
    parcels_gdf['tract_geoid'] = parcels_gdf['geoid20'].apply(lambda x: str(x)[:-1])

    # Use households per parcel as a weight
    # FIXME: use Elmer when data is available
    print('Loading parcel household data...')
    df_parcel_hh = pd.read_csv(r'Y:\Equity Indicators\access\parcels_urbansim.txt', 
                               sep=' ', usecols=['parcelid','hh_p'])
    parcels_gdf = parcels_gdf.merge(df_parcel_hh, left_on='parcel_id', right_on='parcelid')

    # Merge equity share and counts to geographies
    # parcels_gdf['geoid'] = parcels_gdf['geoid20'].astype('int64')
    # equity_shares_blockgroup_df['geoid'] = equity_shares_blockgroup_df['geoid'].astype('int64')
    # parcels_gdf = parcels_gdf.merge(equity_shares_blockgroup_df, on='geoid', how='left')
    # parcels_gdf = parcels_gdf.merge(equity_counts_blockgroup_df, left_on='geoid20', right_on='geoid', how='left')

    parcels_gdf['geoid20'] = parcels_gdf['geoid20'].astype('int64')
    parcels_gdf['tract_geoid'] = parcels_gdf['tract_geoid'].astype('int64')
    equity_shares_blockgroup_df['geoid'] = equity_shares_blockgroup_df['geoid'].astype('int64')
    equity_shares_tract_df['geoid'] = equity_shares_tract_df['geoid'].astype('int64')
    equity_counts_blockgroup_df['geoid'] = equity_counts_blockgroup_df['geoid'].astype('int64')
    equity_counts_tract_df['geoid'] = equity_counts_tract_df['geoid'].astype('int64')
    parcels_gdf_blockgroup = parcels_gdf.merge(equity_shares_blockgroup_df, left_on='geoid20', right_on='geoid', how='left')
    parcels_gdf_blockgroup = parcels_gdf_blockgroup.merge(equity_counts_blockgroup_df, left_on='geoid20', right_on='geoid', how='left')
    parcels_gdf_tract = parcels_gdf.merge(equity_shares_tract_df, left_on='tract_geoid', right_on='geoid', how='left')
    parcels_gdf_tract = parcels_gdf_tract.merge(equity_counts_tract_df, left_on='tract_geoid', right_on='geoid', how='left')

    # Drop any null rows
    print("Removed %s null of %d parcels" %(len(parcels_gdf_tract[parcels_gdf_tract['older_quintile'].isnull()]),len(parcels_gdf_tract)))
    parcels_gdf_tract = parcels_gdf_tract[~parcels_gdf_tract['older_quintile'].isnull()]
    parcels_gdf_blockgroup = parcels_gdf_blockgroup[~parcels_gdf_blockgroup['older_quintile'].isnull()]

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

    results_dict_blockgroup = {}
    results_dict_tract = {}
    for submode, df in submode_dict.items():
        results_dict_blockgroup[submode] = find_nearest(parcels_gdf_blockgroup, df)
        results_dict_blockgroup[submode]['miles'] = results_dict_blockgroup[submode]['dist']/5280.0
        results_dict_tract[submode] = find_nearest(parcels_gdf_tract, df)
        results_dict_tract[submode]['miles'] = results_dict_tract[submode]['dist']/5280.0

    full_output_block_group, block_group_avg_df = aggregate_results(results_dict_blockgroup, 'geoid20')
    full_output_tract, tract_group_avg_df = aggregate_results(results_dict_tract, 'tract_geoid')

    # add a label to wt_avg_miles
    full_output_block_group.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)
    block_group_avg_df.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)
    full_output_tract.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)
    tract_group_avg_df.rename(columns={'wt_avg': 'wt_avg_miles'}, inplace=True)

    # Write to local dir
    full_output_block_group[['aggregation','quintile','access_to','wt_avg_miles']].to_csv(r'distance_indicator_blockgroup.csv', index=False)
    full_output_tract[['aggregation','quintile','access_to','wt_avg_miles']].to_csv(r'distance_indicator_tract.csv', index=False)
    block_group_avg_df.to_csv(r'blockgroup_distance_indicator.csv', index=False) 
    tract_group_avg_df.to_csv(r'tract_distance_indicator.csv', index=False)    

    # Calculate average of an average for the region
    results = {}
    block_group_avg_df = block_group_avg_df.merge(equity_counts_blockgroup_df, left_on='geoid20', right_on='geoid', how='left')
    for agg_col in ['poc', 'income', 'disability','youth', 'older', 'lep']:
        results[agg_col] = {}
        results['non_'+agg_col] = {}
        for submode in ['all_hct','light_rail','commuter_rail','ferry','passenger_ferry','brt']:
            df = block_group_avg_df[(block_group_avg_df['aggregation'] == agg_col) & (block_group_avg_df['access_to'] == submode)]
            df['non_'+agg_col+'_count'] = df['population_total'] - df[agg_col+'_count']
            df['wt_avg_non_'+agg_col] = df['non_'+agg_col+'_count']*df['wt_avg_miles']
            results['non_'+agg_col][submode] = df['wt_avg_non_'+agg_col].sum()/df['non_'+agg_col+'_count'].sum()

            df['wt_avg_'+agg_col] = df[agg_col+'_count']*df['wt_avg_miles']
            results[agg_col][submode] = df['wt_avg_'+agg_col].sum()/df[agg_col+'_count'].sum()

    results_df = pd.DataFrame.from_dict(results)
    results_df.to_csv(r'average_distance_indicator.csv')

if __name__ == '__main__':
    start_time = time.time()
    main()
    print("--- %s minutes ---" % ((time.time() - start_time)/60.0))