library(magrittr)
library(data.table)
library(httr)
library(jsonlite)

# Helper attributes / functions -------------------------------------

races_vector <- c("Asian",                                                                         # Races listed on HMDA
                  "Native Hawaiian or Other Pacific Islander",
                  "Free Form Text Only",
                  "Race Not Available",
                  "American Indian or Alaska Native",
                  "Black or African American",
                  "2 or more minority races",
                  "White",
                  "Joint") %>% lapply(URLencode)                                                   # Recode spaces, etc.

poc_vector <- paste0(races_vector[c(1:3,5:6,9)], collapse=",") %>% c("White") %>%
  lapply(URLencode)

ethnicities_vector <- c("Hispanic or Latino",
                       "Not Hispanic or Latino",
                       "Joint",
                       "Ethnicity Not Available",
                       "Free Form Text Only") %>% lapply(URLencode)

#sexes_vector<- c("Male","Female","Joint","Sex Not Available") %>% lapply(URLencode)

#dwelling_categories_vector <- c("Single Family (1-4 Units):Site-Built",
 #                               "Multifamily:Site-Built",
  #                              "Single Family (1-4 Units):Manufactured",
   #                             "Multifamily:Manufactured") %>% lapply(URLencode)

#income

county_lookup <- data.frame(county_name=c("King","Kitsap","Pierce","Snohomish"),
                            fips=paste0("530",c("33","35","53","61"))) %>%
  rbind(c(county_name="Region",
          fips=c(paste0(.$fips, collapse=",")))) %>%
  setDT() %>% setkey(fips)

hmda_url_builder <- function(dyear,
                             actions_taken,
                             counties="Region",
                             races=NULL,
                             ethnicities=NULL,
                             sexes=NULL,
                             dwelling_categories=NULL){

  url <- paste0("https://ffiec.cfpb.gov/v2/data-browser-api/view/aggregations?counties=",
                 county_lookup[county_name==counties, fips],
                 "&years=", dyear, "&actions_taken=", actions_taken)                               # Multiple years not allowed here, because API sums them unless requested separately

  # if(!is.null(dwelling_categories)){url %<>% paste0("&dwelling_categories=", paste0(dwelling_categories, collapse=","))}
  # if(!is.null(sexes))              {url %<>% paste0("&sexes=",               paste0(sexes, collapse=","))}
  # if(!is.null(ethnicities))        {url %<>% paste0("&ethnicities=",         paste0(ethnicities, collapse=","))}
  if(!is.null(races))               {url %<>% paste0("&races=",               paste0(races, collapse=","))}

  return(url)
}

hmda_api_gofer <- function(pair){                                                                  # Input is a list with 1. URL & 2. datayear; easier vectorizing than separate args
  url    <- pair[[1]]
  dyear  <- pair[[2]]
  resp   <- GET(url)
  result <- fromJSON(content(resp, "text", encoding="UTF-8"),                                      # Text is only option for this API (No json/xml)
                     simplifyDataFrame=TRUE) %>% purrr::pluck("aggregations") %>%
      setDT() %>% .[, year:=(dyear)]                                                               # Add column, as year isn't returned in result
  return(result)
}

# Primary hmda function for Equity Tracker --------------------------

get_hmda <- function(dyears, counties="Region",
                     races=races_vector,
                     ethnicities=NULL,
                     sexes=NULL,
                     dwelling_categories=NULL){
  mixr <- expand.grid(dyear=dyears,                                                                # Notice the API aggregates all specified items
                      actions_taken=c(1,3),                                                        # So years, details must be called individually
                      actions_taken=mixr$actions_taken,
                      races=races,
                      ethnicities=ethnicities,
                      sexes=sexes,
                      dwelling_categories=dwelling_categories
                      )
  rs   <- data.frame(
            mapply(hmda_url_builder,
                   dyear=mixr$dyear,
                   races=mixr$races),
            mixr$dyear) %>%
    transpose() %>% lapply(hmda_api_gofer) %>% rbindlist() %>%
    setDT() %>% setnames("county", "fips") %>% setkey(fips)
  rs %<>% .[county_lookup, county:=county_name] %>%
    dcast(year + county + races ~ actions_taken, value.var="count") %>%
    .[, denial_share:=(`3`/`1`)] %>% setnames(c(`3`,`1`),c("denial_count","full_count"))           # Keep counts for context
  return(rs)
}

## Meg start -----------------------------------------

# Pull csv in from site and download
## MAKE SURE TO ADD COLUMN TO THE LEFT HAND SIDE AND ADD MEDIAN INCOME TO LEFT COLUMN
kitsap_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD14740_BremertonSilverdalePortOrchard.csv")
king_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD42644_SeattleBellevueKent.csv")
pierce_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD45104_TacomaLakewood.csv")

# Clean data file - kitsap
# remove extra columns
kitsap_clean <- kitsap_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
kitsap_clean$denied <- as.numeric(as.character(kitsap_clean$denied))
kitsap_clean$received <- as.numeric(as.character(kitsap_clean$received))

#class(kitsap_clean$received)

# reorganize data and include data year
kitsap_clean$share_denials <- kitsap_clean$denied/kitsap_clean$received
pierce_clean$data_year <- 2021
kitsap_clean$region <- "kitsap"
kitsap_clean <- kitsap_clean[c(6, 7, 1, 2, 3, 4, 5)]

# pivot data set
#test_pivot <- kitsap_clean %>%
 # pivot_longer(cols = c("race_ethnicity"), 
  #             names_to = "equity_groups",
   #            values_to = "identifiers")

# Clean data file - king
# remove extra columns
king_clean <- king_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
king_clean$denied <- as.numeric(as.character(king_clean$denied))
king_clean$received <- as.numeric(as.character(king_clean$received))

#class(king_clean$received)

# reorganize data and include data year
king_clean$share_denials <- king_clean$denied/king_clean$received
pierce_clean$data_year <- 2021
king_clean$region <- "king"
king_clean <- king_clean[c(6, 7, 1, 2, 3, 4, 5)]

# Clean data file - pierce
# remove extra columns
pierce_clean <- pierce_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
pierce_clean$denied <- as.numeric(as.character(pierce_clean$denied))
pierce_clean$received <- as.numeric(as.character(pierce_clean$received))

#class(pierce_clean$received)

# reorganize data and include data year
pierce_clean$share_denials <- pierce_clean$denied/pierce_clean$received
pierce_clean$data_year <- 2021
pierce_clean$region <- "pierce"
pierce_clean <- pierce_clean[c(6, 7, 1, 2, 3, 4, 5)]

# join datasets across region -----------------------------------------

region_clean <- rbind(kitsap_clean, king_clean, pierce_clean)

# plot data -----------------------------------------

bar_chart <- static_bar_chart(t=just_region_11,
                              x="fact_value", y="focus_attribute_ord",
                              fill="tenure",
                              est = "percent",
                              pos= "stack",
                              color = "psrc_light", 
                              title = paste0(indicator_title, ": King (2011)"),
                              subtitle = "by equity focus group",
                              source = "U.S. Census Bureau, American Community Survey (ACS) 5-Year Public Use Microdata Sample (PUMS)")
bar_chart

# Export data

#work_book <- createWorkbook()
#addWorksheet(work_book, sheetName = "median sale price")
#writeData(work_book, "median sale price", county_piv)
#saveWorkbook(work_book, file = "Affordability Measures/r_output median_price_county.xlsx", overwrite = TRUE)
