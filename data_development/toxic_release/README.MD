This script uses point source toxic release data and creates average per-preson exposure rates for each tract in the region. Sites are buffered and then intersected with parcels that have yearly OFM population estimates. Overlapping buffers are summed so that population in proximity to multiple sites is handled correctly. The parcels are then grouped by tracts and a weighted per-capita exposure is calculated for tract.  

# Setup
Clone this repository to a local directory. Open an Anaconda prompt and change directory to the location of the local repository (equity-tracker/data_development/toxic_release). 

## Virtual Environment
From the Anaconda prompt in this project directory root, enter the following to install a virtual environment that includes all required versions of Python libraries:
 - `conda env create -f environment.yaml`

After install is complete enter: 
- `conda activate toxi_release_env`. 
You should see (toxic_release_env) in the prompt. This indicates that the prompt is using all the libraries associated with the virtual environment. This environment will need to be activated using this command any time a new prompt is opened.

## Elmer Connections
Python connections to Elmer and ElmerGeo databases require an ODBC driver to connect with SQL. To set this up, [download the ODBC Driver 17 here](https://go.microsoft.com/fwlink/?linkid=2200732) and follow the install instructions, selecting all defaults.

# Usage
Configuration settings for the script are controlled by **config.**. The main settings to update here is `toxic_release_data_path`, which the source dir of the toxic release data and output_path, which is where the output file will be stored. Optionaly, the user can export shapefiles for one year of data. These include the toxic release point locations and buffers. The script will run for each file in this dir, where each file represents a year. 

## Scripts
The main script is controlled through `toxic_release_exposure.py` and specifying the location of the congif file by using the '-c' flag. For example:
- `python toxic_release_exposure.py -c C:\temp\equity-tracker\data_development\toxic_release`  (with the toxic_releae_env virtual environment activated)
