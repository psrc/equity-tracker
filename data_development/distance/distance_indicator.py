import os, sys, time
import numpy as np
import pandas as pd
import geopandas as gpd
from scipy.spatial import cKDTree
# from pymssql import connect
import warnings
import transit_service_analyst as tsa
warnings.filterwarnings('ignore')

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

def aggregate_results(results_dict, geog_level, analysis_year, 
                      gtfs_year, ofm_estimate, ofm_vintage):

    # Iterate over submodes
    full_output_df = pd.DataFrame()
    for submode in results_dict.keys():
        # Calculate a weighted average for each tract, weighting by number of people in geog_level
        df = weighted_avg(results_dict[submode], 'miles', 'household_pop', geog_level)
        df = df.reset_index()
        df['access_to'] = submode
        df['gtfs_year'] = gtfs_year
        df['ofm_estimate_year'] = ofm_estimate
        df['ofm_vintage'] = ofm_vintage
        df['geoid_value'] = geog_level.split('_')[-1]
        df['analysis_year'] = analysis_year
        full_output_df = pd.concat([full_output_df,df.reset_index()])

    return full_output_df

def fetch_gtfs(config, analysis_year):
    """Load transit stop and route data from GTFS and return selected high-capacity submodes stops."""

    gtfs_dir = os.path.join(config["gtfs_path"], str(analysis_year))
    # GTFS only available back to 2015; if earlier, use older available year
    available_gtfs_years = []
    for entry_name in config["analysis_year_list"]:
        if str(entry_name) in os.listdir(config["gtfs_path"]):
            entry_path = os.path.join(config["gtfs_path"], str(entry_name))
            if os.path.isdir(entry_path):
                available_gtfs_years.append(int(entry_name))

    # Get closest available GTFS data 
    gtfs_year = return_max_min(analysis_year, available_gtfs_years)

    # Set path of GTFS data to use
    gtfs_dir = os.path.join(config["gtfs_path"], str(gtfs_year))

    # Get service date from calendar.txt
    df_calendar = pd.read_csv(os.path.join(gtfs_dir,'calendar.txt'))
    service_date = str(df_calendar['start_date'].iloc[0]+1)   # +1 because the service dates are between start_date and end_date

    transit_analyst = tsa.load_gtfs(gtfs_dir, service_date)
    route_lines = transit_analyst.get_lines_gdf()
    # Ensure all route ID's are lowercase
    route_lines['route_id'] = route_lines['route_id'].apply(lambda x: x.lower())
    # Routes include both directions; choose the first instance
    route_lines = route_lines.groupby('route_id').first().reset_index()
    # Route is HCT submode OR is in list of BRT routes (and classified as generic bus route type)
    hct_routes = route_lines[route_lines['route_type'].isin(config["hct_submode_list"]) | route_lines['route_id'].isin(config['brt_routes'].keys())]
    route_stops = transit_analyst.get_line_stops_gdf()
    # Ensure all route ID's are lowercase
    route_stops['route_id'] = route_stops['route_id'].apply(lambda x: x.lower())

    gdf = route_stops[route_stops['route_id'].isin(hct_routes['route_id'])]
    gdf = gdf.merge(hct_routes[['route_id','route_type']], on='route_id', how='left')
    gdf = gdf.to_crs(config["crs"])

    gdf['submode'] = gdf['route_type'].astype('str').map(config['route_type_map'])

    gdf.geometry = gdf.centroid   # Should already be a point, but take centroid just in case

    return gdf, gtfs_year

#######################################################
# Calculate average weighted distance to features
#######################################################

def load_parcel_data(config, elmer_engine):
    """Load parcel data as geodataframe. This is a large dataset and gets reused as we iterate through analysis years."""

    parcel_geog_df = pd.read_sql(sql='select parcel_id, tract_geoid10, tract_geoid20, \
                                 x_coord_state_plane, y_coord_state_plane \
                                 from small_areas.parcel_dim where base_year='+str(config['base_year']), 
                                 con=elmer_engine)
    parcels_gdf = gpd.GeoDataFrame(
        parcel_geog_df, geometry=gpd.points_from_xy(parcel_geog_df.x_coord_state_plane, 
        parcel_geog_df.y_coord_state_plane), crs=config["crs"]
        )
    
    return parcels_gdf

def return_max_min(requested_value, available_value_list):
    """Return nearest value in a list, either the max or min if the value is not in the list."""

    if requested_value < min(available_value_list):
        returned_value = min(available_value_list)
    elif requested_value > max(available_value_list):
        returned_value = max(available_value_list)
    elif requested_value in available_value_list:
        returned_value = requested_value
    else: 
        return None

    return returned_value

def calc_hct_distance(config, analysis_year, parcels_gdf, elmer_engine):
     
    start_time = time.time()

    # Define census geographies based on analysis year
    if int(analysis_year) >= 2020:
        geoid = "geoid20"
    else:
        geoid = "geoid10"

    # Load GTFS data and return HCT data
    hct_gdf, gtfs_year = fetch_gtfs(config, analysis_year)  
    
    # Use household population per parcel as a weight
    # Load this from OFM estimates (using the same estimate year and OFM vintage year, 
    # (e.g. 2017 OFM release for 2017 estimates, 2023 OFM release for 2023 estimates)
    # 
    estimate_year_list =  pd.read_sql("select DISTINCT estimate_year from ofm.parcelized_saep_facts",
                                  con=elmer_engine)
    ofm_vintage_list =  pd.read_sql("select DISTINCT ofm_vintage from ofm.parcelized_saep_facts",
                                  con=elmer_engine)
    df_ofm_years = pd.read_sql("select distinct ofm_vintage, estimate_year \
                from ofm.parcelized_saep_facts f  \
                order by ofm_vintage, estimate_year",
                con=elmer_engine)
    
    # Select nearest available estimate year and vintage
    # If estimate year is available use that
    if len(df_ofm_years[df_ofm_years['estimate_year'] == analysis_year]) > 0:
        ofm_estimate = analysis_year
    # If estimate year is not available use the nearest available (newest or oldest depending on analysis year)
    else:
        ofm_estimate = min(df_ofm_years['estimate_year'], key=lambda x:abs(x-analysis_year))
        
    # For whatever ofm year selected, use the newest available vintage
    ofm_vintage = df_ofm_years[df_ofm_years['estimate_year'] == ofm_estimate]['ofm_vintage'].max()

    parcel_pop_df = pd.read_sql(sql="select parcel_id, household_pop from ofm.parcelized_saep("+ \
                                str(ofm_vintage)+", "+str(ofm_estimate)+")", 
                                con=elmer_engine)
    parcels_gdf = parcels_gdf.merge(parcel_pop_df, on='parcel_id')
    parcels_gdf['tract_'+geoid] = parcels_gdf['tract_'+geoid].astype('int64')

    #######################################################
    # Access to Transit Stations
    #######################################################

    print('Calculating weighted distance...')

    # Select geodataframes of stops for each submode
    submode_dict = {'all_hct': hct_gdf,
                    'light_rail': hct_gdf[hct_gdf['submode'] == 'light_rail'],
                    'commuter_rail': hct_gdf[hct_gdf['submode'] == 'commuter_rail'],
                    'ferry': hct_gdf[hct_gdf['submode'] == 'ferry'],
                    'brt' : hct_gdf[hct_gdf['submode'] == 'brt']
    }

    # Iterate through submodes and calculate nearest stop (by submode) for each parcel
    results_dict_tract = {}
    for submode, gdf in submode_dict.items():
        print(submode)
        if len(gdf) > 0:
            results_dict_tract[submode] = find_nearest(parcels_gdf, gdf)
            results_dict_tract[submode]['miles'] = results_dict_tract[submode]['dist']/5280.0
        else:
            print('no routes for submode: '+ submode)

        # Write gdf to file
        if len(gdf) > 0:
            gdf_out = gdf.groupby(['stop_id','route_id']).first()
            gdf_out.to_file(os.path.join(config['output_dir'], str(analysis_year), f"{submode}.shp"))

    # Aggregate parcel-level distances to population-weighted averages at the tract level
    tract_output_df = aggregate_results(results_dict_tract, 'tract_'+geoid, analysis_year, 
                                        gtfs_year, ofm_estimate, ofm_vintage)

    # Explicitly label distance as miles
    tract_output_df.rename(columns={'wt_avg': 'wt_avg_miles', 'tract_'+geoid: 'tract_geoid'}, inplace=True)

    return tract_output_df

    print("--- %s minutes ---" % ((time.time() - start_time)/60.0))