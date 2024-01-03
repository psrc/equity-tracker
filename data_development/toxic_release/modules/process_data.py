import pandas as pd
import geopandas as gpd
from shapely.ops import polygonize
import pyodbc


def read_from_elmer_odbc(connection_string: str, query_string: str) -> pd.DataFrame:
    """Returns a Pandas DataFrame from Elmer using the connection string and sql query_string

    Args:
        connection_string (str): Valid connection string to Elmer Database
        query_string (str): SQL statement

    Returns:
        pd.DataFrame: SQL query results as a Pandas DataFrame
    """

    sql_conn = pyodbc.connect(connection_string)
    return pd.read_sql(sql=query_string, con=sql_conn)


def non_overlapping_buffer(buffers: gpd.GeoDataFrame, buff_id) -> gpd.GeoDataFrame:
    """Ruturns unique, non-overlapping polygons from input polygons (overlapping buffers)

    Args:
        buffers (gpd.GeoDataFrame): overlapping  polygons
        buff_id (_type_): an unique ID for each polygon

    Returns:
        gpd.GeoDataFrame: non-overlapping polygons/buffers
    """

    no_overlaps = gpd.GeoDataFrame(
        geometry=list(polygonize(buffers.exterior.unary_union))
    )

    no_overlaps[buff_id] = range(1, 1 + len(no_overlaps))
    no_overlaps.crs = buffers.crs
    return no_overlaps


def aggregate_non_overlaps(
    points: gpd.GeoDataFrame,
    buffer_size: int,
    sum_fields: list,
    buff_id="non_ov_buff_id",
) -> gpd.GeoDataFrame:
    """Buffers points and calls non_overlapping_buffer to create non_overlapping polygons. 
        Then intersects with original buffers and sums sum_fields to get sum values for 
        all unique polygons.

    Args:
        points (gpd.GeoDataFrame): points representing a site.
        buffer_size (int): size of buffer in feet
        sum_fields (list): fields that will be summed for all polys including overlaps.
        buff_id (str, optional): An ID for each non-overlapping poly/buffer. Defaults to "non_ov_buff_id".

    Returns:
        gpd.GeoDataFrame: non-overlapping polygons/buffers with summed values
    """

    # buffer points
    buffered_points = points.copy()
    buffered_points["geometry"] = points.buffer(buffer_size)

    # deal with overlaps. get unique polygons.
    buffers_no_overlaps = non_overlapping_buffer(buffered_points, buff_id)
    buff_intersect = gpd.overlay(buffers_no_overlaps, buffered_points)

    # get rid of very small polygons:
    buff_intersect = buff_intersect[buff_intersect.geometry.area > 0.000001]

    keep_list = sum_fields + [buff_id]

    buff_intersect = buff_intersect[keep_list]

    agg_dict = {sum_field: "sum" for sum_field in sum_fields}

    buff_intersect = buff_intersect.groupby(buff_id).agg(agg_dict).reset_index()

    buffers_no_overlaps = buffers_no_overlaps.merge(
        buff_intersect, how="left", on=buff_id
    )

    return buffers_no_overlaps


def get_ofm_parcelized(
    year: int, vintage: int, census_geog: str, connection_string: str, crs: int
) -> gpd.GeoDataFrame:
    """Returns a Pandas DataFrame with parcels and OFM data. See sql query in the function for more details

    Args:
        year (int): year to get OFM data
        vintage (int): used to determine data vintage
        census_geog (str): cenus_geog: tract, block_group or block
        connection_string (str): Valid connection string to Elmer Database
        crs (int): cordinate system for parcels

    Returns:
        gpd.GeoDataFrame: Parcelized OFM data from Elmer.
    """
    if year < 2020:
        vintage = 2020

    sql_str = f"""
        select psf.total_pop, pd.parcel_id, pd.{census_geog}_geoid10, pd.{census_geog}_geoid20, 
            pd.x_coord_state_plane, pd.y_coord_state_plane 
        FROM ofm.parcelized_saep_facts as psf join small_areas.parcel_dim as pd on 
            psf.parcel_dim_id = pd.parcel_dim_id 
        WHERE psf.ofm_vintage = {vintage} AND psf.estimate_year = {year}"""

    gdf = read_from_elmer_odbc(connection_string, sql_str)

    gdf = gpd.GeoDataFrame(
        gdf,
        geometry=gpd.points_from_xy(
            gdf["x_coord_state_plane"], gdf["y_coord_state_plane"]
        ),
        crs=crs,
    )
    return gdf


def census_geog_weighted(
    parcels: gpd.GeoDataFrame,
    columns: list,
    polys: gpd.GeoDataFrame,
    census_geog: str,
    year: str,
) -> pd.DataFrame:
    """Using parcelized OFM data, weights data by population and then aggregates to the specfied census_geog.

    Args:
        parcels (gpd.GeoDataFrame): parcelized OFM data
        columns (list): list of colum names in polys that has data to be weighted and aggregated to census_geog
        polys (gpd.GeoDataFrame): polygon file that contains columns that are to weighed by parcelized OFM data.
        census_geog (str): cenus_geog: tract, block_group or block
        year (str): year being processed

    Returns:
        pd.DataFrame: aggregated weighted measures.
    """
    if int(year) < 2020:
        census_id = 'geoid10'
    else:
        census_id = 'geoid20'
    df = gpd.sjoin(parcels, polys, how="left")
    df.fillna(0, inplace=True)
    # add pop sum for each tract:
    df[f"{census_geog}_total_pop"] = (
        df["total_pop"].groupby(df[f"{census_geog}_{census_id}"]).transform("sum")
    )

    agg_dict = {}

    for col_name in columns:
        new_col = f"exposure"
        df[new_col] = (df["total_pop"] * df[col_name]) / df[f"{census_geog}_total_pop"]
        agg_dict[new_col] = "sum"
    df = df.groupby(f"{census_geog}_{census_id}", as_index=False).agg(agg_dict)
    df['year'] = int(year)
    df = df.rename(columns={f"{census_geog}_{census_id}" : 'geoid'})
    
    return df


def process(config, gdf, year):
    """process to buffer points, then aggregate population weighted measures to a census geography.

    Args:
        config (Config): config file
        gdf (gdf.GeoDataFrame): points to buffer
        year (int): year to process data
        vintage (int): used for Parcel/OFM vintage

    Returns:
        pd.DataFrame: weighted measures aggregated to census geog.
    """
    no_overlap_aggregation = aggregate_non_overlaps(
        gdf, config.buffer_size, config.toxic_release_columns
    )

    ofm_parcels = get_ofm_parcelized(
        year, config.vintage, config.census_geog, config.elmer_conn_string, config.crs
    )

    weighted = census_geog_weighted(
        ofm_parcels,
        config.toxic_release_columns,
        no_overlap_aggregation,
        config.census_geog,
        str(year),
    )

    return weighted
