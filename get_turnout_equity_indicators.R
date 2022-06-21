library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(sf)
library(tidycensus)
library(openxlsx)

# References -------------------------------------------------------
dyear <- 2020
voting_url <-"https://www.sos.wa.gov/_assets/elections/research/"
turnout_url <- paste0(voting_url, dyear, "Gen_Precinct_Results_GIS-Ready.xlsx")
psrc_counties <- data.frame("CountyName"=c("King","Kitsap","Pierce","Snohomish"),
                            "FIPS"=c("033","035","053","061"))

# Helper functions -------------------------------------------------
sandbox_connect <- function(){DBI::dbConnect(odbc::odbc(),
                                           driver = "ODBC Driver 17 for SQL Server",
                                           server = "AWS-PROD-SQL\\Sockeye",
                                           database = "Sandbox",
                                           trusted_connection = "yes",
                                           port = 1433)
}


# Download/unzip precinct geometry
fetch_zip <- function(dyear){
  temp <- tempfile()
  filename <- paste0("Statewide_Precincts_", dyear, "General")
  zip_url <- paste0(voting_url, filename, ".zip")
  download.file(zip_url, temp)
  unzip(temp, exdir="~/precincts")
  sf_df <- paste0("precincts/", filename, ".shp") %>% st_read() %>%
    filter(CountyName %in% psrc_counties$CountyName) %>%
    st_transform(2285)
  unlink(temp)
  rm(temp)
  return(sf_df)
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

# Primary function --------------------------------------------------

## Determine tract EFA concentration for each: POC, low income, disability, low English proficiency
#XXX

precinct_sql <- "SELECT * FROM Mike.block20_to_precinct;"
sandbox_connection <- sandbox_connect()
b2p <- DBI::dbGetQuery(sandbox_connection, DBI::SQL(precinct_sql)) %>% setDT()
DBI::dbDisconnect(sandbox_connection)

vt <- read.xlsx(turnout_url) %>% setDT() %>%                                                       # Load turnout data
  .[County %in% psrc_counties$CountyName & RaceName=="Turnout"] %>%
  dcast(PrecinctCode ~ Candidate, value.var="Votes")

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
cor.test(vt$turnout_share, vt$POC_voter_share,                                                     # Run correlation
         method = "pearson",
         conf.level = 0.90)

# Display mean turnout by EFA concentration quintile

