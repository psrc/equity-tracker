library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(sf)
library(tidycensus)
library(openxlsx)
library(psrcelmer)

# References -------------------------------------------------------
dyear <- 2020
voting_url <-"https://www.sos.wa.gov/sites/default/files/2024-02/"
turnout_zip_url <- paste0(voting_url, "2020Gen_Precinct_Results_GIS-Ready.zip")
psrc_counties <- data.frame("CountyName"=c("King","Kitsap","Pierce","Snohomish"),
                            "FIPS"=c("033","035","053","061"))

# Helper functions -------------------------------------------------

# Download/unzip precinct geometry
fetch_zip <- function(dyear){
  temp <- tempfile()
  filename <- paste0("Statewide_Precincts_", dyear, "General")
  zip_url <- paste0("https://www.sos.wa.gov/_assets/elections/research/", filename, ".zip")
  download.file(zip_url, temp)
  unzip(temp, exdir="~/precincts")
  sf_df <- paste0("precincts/", filename, ".shp") %>% st_read() %>%
    filter(CountyName %in% psrc_counties$CountyName) %>%
    st_transform(2285)
  unlink(temp)
  rm(temp)
  return(sf_df)
}

# Download and extract turnout data from zip file
fetch_turnout <- function(){
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  
  # Download the zip file
  download.file(turnout_zip_url, temp_zip)
  
  # Extract the zip file
  unzip(temp_zip, exdir = temp_dir)
  
  # Read the Excel file
  excel_file <- file.path(temp_dir, "2020Gen_Precinct_Turnout_GIS-Ready.xlsx")
  turnout_data <- read.xlsx(excel_file)
  
  # Clean up temporary files
  unlink(temp_zip)
  unlink(excel_file)
  
  return(turnout_data)
}

# Retrieve Census redistricting block population & geometry
fetch_blockpop <- function(dyear){
  x <- get_decennial(geography="block",
                     variables=c("P3_001N","P3_002N"),
                     year=dyear,
                     sumfile="pl",
                     state="53",
                     county=c("033","035","053","061"),
                     geometry=FALSE,
                     output="wide",
                     key=Sys.getenv("CENSUS_API_KEY")) %>%
    mutate(voting_age=as.integer(P3_001N),
           POC=as.integer(P3_001N-P3_002N)) %>%
    select(-c(P3_001N,P3_002N)) #%>% st_transform(2285)
  return(x)
}

label_quintile <- function(var){
  breakpoints <- unique(quantile(var, probs=0:5/5, na.rm=TRUE))                                    # Unique so all-zero quintiles act as one bin
  rv <- cut(var, breaks=unique(breakpoints), labels=1:(length(breakpoints)-1),
            include.lowest=TRUE, right=TRUE)
  return(rv)
}

# Primary function --------------------------------------------------

b2p <- psrcelmer::get_table(db_name="Sandbox", schema="Mike", tbl_name="block20_to_precinct")      # Spatial join much faster in SQL

# Load turnout data using new source and column names
vt <- fetch_turnout() %>% setDT() %>%                                                              # Load turnout data from zip
  .[County %in% psrc_counties$CountyName] %>%                                                      # Filter to PSRC counties
  .[, .(County, PrecinctCode = PrecCode, PrecName, 
        `Registered Voters` = G20TREGVOT, `Ballots Cast` = G20TBALCST)]                            # Rename columns to match original code

# precincts <- fetch_zip(dyear)                                                                    # Fetch precinct geometry
# precincts %<>% inner_join(vt, by=c("St_Code"="PrecinctCode"))                                    # -- link to turnout

block_pop <- fetch_blockpop(dyear) %>%                                                             # Fetch block-level redistricting population estimates
  setDT() %>% .[b2p, precinct_code:=st_code, on=.(GEOID=geoid20)]                                  # Associate precinct
precinct_pop <- block_pop[!is.na(precinct_code), lapply(.SD, sum),
                          by=.(county=str_sub(GEOID,3,5), precinct_code), .SDcols=c("voting_age", "POC")] # Summarize to precinct
vt[precinct_pop, `:=`(county=county, voting_age=voting_age, POC=POC),
   on=.(PrecinctCode=precinct_code)]                                                               # Combine data items
vt[, `:=`(POC_voter_share=as.numeric(POC)/voting_age, turnout_share=`Ballots Cast`/voting_age)]    # Create analysis variables
vt[POC_voter_share>1,POC_voter_share:=1]                                                           # Topcode
vt[turnout_share>1,turnout_share:=1]                                                               # Topcode
# cor.test(vt$turnout_share, vt$POC_voter_share,                                                     # Run correlation
#          method = "pearson",
#          conf.level = 0.90)

# Display mean turnout by EFA concentration quintile
vt[,POC_bin:=factor(label_quintile(POC_voter_share), levels=c(1:5))]                               # Add quintile
rs <- vt[!is.na(POC_bin), .(avg_turnout=mean(turnout_share), .N), by=.(POC_bin)] %>%               # Summarize
  setorder(POC_bin)