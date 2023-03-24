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
                  "Joint") %>% lapply(URLencode)

poc_vector <- paste0(races_vector[c(1:3,5:6,9)], collapse=",") %>% c("White") %>%
  lapply(URLencode)

# ethnicities_vector <- c("Hispanic or Latino",
#                       "Not Hispanic or Latino",
#                       "Joint",
#                       "Ethnicity Not Available",
#                       "Free Form Text Only") %>% lapply(URLencode)
#
# sexes_vector<- c("Male","Female","Joint","Sex Not Available") %>% lapply(URLencode)
#
# dwelling_categories_vector <- c("Single Family (1-4 Units):Site-Built",
                                # "Multifamily:Site-Built",
                                # "Single Family (1-4 Units):Manufactured",
                                # "Multifamily:Manufactured") %>% lapply(URLencode)

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
  # if(!is.null(ethnicities))        {url %<>% paste0("&ethnicities=",         paste0(ethnicities, collapse=","))}
  if(!is.null(races))               {url %<>% paste0("&races=",               paste0(races, collapse=","))}

  return(url)
}

hmda_api_gofer <- function(pair){
  url    <- pair[[1]]
  dyear  <- pair[[2]]
  resp   <- GET(url)
  result <- fromJSON(content(resp, "text", encoding="UTF-8"),                                      # No json/xml option for this API
                     simplifyDataFrame=TRUE) %>% purrr::pluck("aggregations") %>%
      setDT() %>% .[, year:=(dyear)]
  return(result)
}

# Primary hmda function for Equity Tracker --------------------------

get_hmda <- function(dyears, races=races_vector){
  mixr <- expand.grid(dyear=dyears, actions_taken=c(1,3), races=races)                             # Notice the API aggregates all specified items
  rs   <- data.frame(
            mapply(hmda_url_builder,
                   dyear=mixr$dyear,                                                               # So years, details must be called individually
                   actions_taken=mixr$actions_taken,
                   races=mixr$races),
            mixr$dyear) %>%
    transpose() %>% lapply(hmda_api_gofer) %>% rbindlist() %>%
    setDT() %>% setnames("county", "fips") %>% setkey(fips)
  rs %<>% .[county_lookup, county:=county_name] %>%
    dcast(year + county + races ~ actions_taken, value.var="count") %>%
    .[, denial_share:=(`3`/`1`)] %>% .[, c("3","1"):=NULL]
  return(rs)
}

# Example
denial_rate_x_race <- get_hmda(2018:2021)

#url <- paste0("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv?counties=53033&years=2018")
