This method calculates average weighted distance to high capacity transit (HCT) stations for each tract in the region, using parcel-level household population as weights. The setup, usage, and calculation methods are described below. 

# Setup
Clone this repository to a local directory.* Open an Anaconda prompt and change directory to the location of the local equity-tracker repository. 

*If you have been working on this project, you will have likely already cloned this repository to a local directory. 

_As an example:_

![Capture](https://github.com/user-attachments/assets/a96ae4bd-e3fc-4199-9000-02125087181a)

## Virtual Environment
From the Anaconda prompt in the equity-tracker directory root, enter the following to install a virtual environment that includes all required versions of Python libraries:
 - `conda env create -f data_development\distance\environment.yml`

After install is complete enter: 
- `conda activate equity_tracker_env`. 
You should see (equity_tracker_env) in the prompt. This indicates that the prompt is using all the libraries associated with the virtual environment. This environment will need to be activated using this command any time a new prompt is opened.

## Elmer Connections
Most Data staff should already have access to work with Elmer databases. Check that the ODBC driver is installed on your workspace by searching for it in Windows - it should show up as ODBC Data Sources (64-bit). If not, [download the ODBC Driver 17 here](https://go.microsoft.com/fwlink/?linkid=2200732) and follow the install instructions, selecting all defaults. If there are problems connecting with Elmer, contact Brice or Chris Peak. 

# Usage
Configuration settings for the script are controlled by **configuration.toml**. The main setting to update here is `analysis_year_list`, which controls the years the tool should update. When running this for newly available data, it should be sufficient to create a list of this year only (e.g., [2024]). The `output_dir` specifies where results will be available and is set by default to `Y:/Equity Indicators/access`. In general, no other settings should need to be changed. 

## Input Data
This script will access the latest data available for a given year by searching in Elmer for the appropriate year and using the nearest year when a specific year is not available. 

Most inputs do not need to be updated manually, with the exception of GTFS data. PSRC staff (Craig/Grant) usually update GTFS feeds at the location used by this script (see `gtfs_path` in configuration.toml which points to `X:/DSA/GTFS/spring`), but the user should confirm the data is available. Again if the given year isn't there, the script will still run but will use the most recent year. The output file reports which year was used for all data, including GTFS.

In addition to ensuring GTFS data is available for the analyis year, users should also check that bus rapid transit (BRT) route definitions are updated in the **brt_routes** list in configuration.toml. Since GTFS does not separate BRT from local service, we provide a list of route IDs and names to include with HCT service. The following should include most planned service until Sound Transit's Stride service starts later in the 2020s. 

Ensure that no new BRT routes should be included to the following. If so, check the routes.txt file in the GTFS data to determine the route ID and name and add to the **brt_routes** list.

'kc_100512' = 'A Line'
'kc_102548' = 'B Line'
'kc_102576' = 'C Line'
'kc_102581' = 'D Line'
'kc_102615' = 'E Line'
'kc_102619' = 'F Line'
'kc_102736' = 'H Line'
'ct_701' = 'Swift Blue'
'ct_702' = 'Swift Green'

Note that this list is applicable to all years, before these were all in service. The script selects the lines from GTFS only if they're available, so the list should be as current as possible regardless of which is being processed.

## Scripts
The main script is controlled through `generate_indicators.py`, which can be run after verifying settings in `configuration.toml` by running:
- `python data/development/distance/generate_indicators.py` (with the equity-tracker virtual environment activated)

This script will process the list of analysis years and store results at the specified output directory. A folder is generated for each analysis year with a file inside named `tract_hct_distance.csv`. 

|index|tract_geoid|wt_avg_miles       |access_to|gtfs_year|ofm_estimate_year|ofm_vintage|geoid_value  |analysis_year|
|-----|-------------|-------------------|---------|---------|-----------------|-----------|-------------|-------------|
|0    |53033000101  |2.373904774499783  |all_hct  |2023     |2023             |2023       |geoid20|2023         |
|1    |53033000102  |2.276320101062316  |all_hct  |2023     |2023             |2023       |geoid20|2023         |
|2    |53033000201  |1.4261937878008994 |all_hct  |2023     |2023             |2023       |geoid20|2023         |
|3    |53033000202  |1.9068508927791523 |all_hct  |2023     |2023             |2023       |geoid20|2023         |
|4    |53033000300  |0.44945824419725416|all_hct  |2023     |2023             |2023       |geoid20|2023         |

This file reports the average weighted miles to a variety of HCT stations within each tract. The script calculates distances for all HCT stops, but also includes rows for individual HCT modes (light rail, commuter rail, ferry, BRT) labeled by the `access_to` field. The remaining columns are identifiers to show which version of data was used.* Ideally, these should all match the analysis_year.** The `geoid_value` indicates which Census vintage the tracts comprise (geoid20 for 2020 and geoid10 for 2010 geographies).

*Although the full data set generated in the following step includes years back to 2011, GTFS data is only available back to 2015.  

**The various year fields can be confusing because they may not match the `analysis_year`. The `ofm_estimate_year` (year data is from) should match the `analysis_year` (whichever year you're trying to get the data from). The only time that those two would be different is if an ofm estimate isn't available for that analysis year. `ofm_estimate_year` is different from `ofm_vintage` (which represents the year from which the `ofm_estimate_year` was produced). There are multiple OFM vintages of when the data was published, but this script searches for the when the newest available vintage was released, so the year is sometimes more recent than the `ofm_estimate_year`.

Once the individual years are produced, this script can be run to combine all years into a single file:
- `python data_development\distance\compile_results.py`

This script concatenates all yearly output files into a single file named **tract_hct_distance_compiled.csv**, which should serve as the primary method for accessing this data. 

# Calculation Method
The output of this script is an average weighted distance to HCT stops for people in each Census tract. The calculation is performed by finding the straight-line distance to the nearest HCT stop for each parcel. HCT stop data is based on annual, regional GTFS data. Outputs are available for each submode (e.g., average distance to light rail stations) and all HCT stops, which means whichever is nearest of all submodes. 

After determining the distance to the nearest stop for each parcel, the household population at the parcel is used as a weight to calculate an average distance for all people within a tract. This ensures that uneven population distributions and parcels with multiple households located on them (e.g., apartments) are accurately representing access for a tract, which can be a large geographic area. Household population is taken from OFM estimates at the parcel level, from whichever year is available, using the latest OFM vintage whenever available. 
