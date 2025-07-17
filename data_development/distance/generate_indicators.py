import pandas as pd
import os
from pathlib import Path
import sqlalchemy
from sqlalchemy.engine import URL
import distance_indicator
import toml

config = toml.load("data_development\distance\configuration.toml")

# Connections to Elmer
connection_url = URL.create("mssql+pyodbc", 
                            query={"odbc_connect": config["elmer_conn_string"]})
elmer_engine = sqlalchemy.create_engine(connection_url)

# Load parcels as geodataframe
parcels_gdf = distance_indicator.load_parcel_data(config, elmer_engine)

for analysis_year in config['analysis_year_list']:

    print("processing year: " + str(analysis_year))

    # Create output directory if needed
    if not os.path.exists(os.path.join(config['output_dir'], str(analysis_year))):
        os.makedirs(os.path.join(config['output_dir'], str(analysis_year)))
    output_df = distance_indicator.calc_hct_distance(config, analysis_year, parcels_gdf, elmer_engine)

    output_df.to_csv(os.path.join(config['output_dir'], str(analysis_year), 'tract_hct_distance.csv'), index=False)  