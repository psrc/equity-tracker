import os
import pandas as pd
import toml

config = toml.load("configuration.toml")

results_df = pd.DataFrame() 

for analysis_year in config['analysis_year_list']:
    df = pd.read_csv(os.path.join(config['output_dir'], str(analysis_year), 'tract_hct_distance.csv'))
    results_df = pd.concat([results_df, df])

results_df.to_csv(os.path.join(config['output_dir'],'tract_hct_distance_compiled.csv'))
    