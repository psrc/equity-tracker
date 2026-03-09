from modules import process_data
import yaml
from pathlib import Path
from modules import configuration
import geopandas as gpd
import pandas as pd


file = Path().joinpath(configuration.args.configs_dir, "config.yaml")

config = yaml.safe_load(open(file))
config = configuration.Config(config)

file_paths = [
    f for f in Path(config.toxic_release_data_path).iterdir() if f.suffix == ".csv"
]

max_year = max([int(f.stem[:4]) for f in file_paths])

data_list = []

for file_path in file_paths:
    year = int(file_path.stem[:4])

    # get the data"
    df = pd.read_csv(file_path)
    df = df.rename(columns=config.column_dict)

    # keep only the columns specified in the config
    df = df[list(config.column_dict.values())]

    # revoves duplciate columns, which happens becuase same value is used twice in config.column_dict

    # make sure there is one set of coords for each site
    assert len(df["lat"].unique()) <= len(df["frs_id"].unique())
    assert len(df["long"].unique()) <= len(df["frs_id"].unique())

    # sum toxic release columns by site id, get first record for all others
    agg_dict = {
        col: "sum" if col in config.toxic_release_columns else "first"
        for col in df.columns
    }

    # delete the grouping variable:
    del agg_dict[config.site_id_column_name]

    # aggregate by site:
    df = df.groupby(config.site_id_column_name).agg(agg_dict).reset_index()

    # convert to GeoDataFrame of points
    gdf = gpd.GeoDataFrame(
        df, geometry=gpd.points_from_xy(df["long"], df["lat"]), crs="EPSG:4326"
    )

    # convert to specified coordinate system
    gdf = gdf.to_crs(config.crs)

    if config.export_shapefiles and config.export_year == year:
        df = process_data.process(config, gdf, year, True, config.output_path)
    else:
        df = process_data.process(config, gdf, year, False)
    assert len(df) > 0
    data_list.append(df)

    print(f"finished processing {str(year)}")
df = pd.concat(data_list, axis=0)
# df.reset_index(inplace = True)
df.to_csv(Path(config.output_path) / config.output_file_name, index=False)
print("done")
