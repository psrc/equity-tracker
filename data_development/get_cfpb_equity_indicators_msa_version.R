library(magrittr)
library(data.table)
library(httr)
library(jsonlite)

races <- c("Asian",
           "Native Hawaiian or Other Pacific Islander",
           "Free Form Text Only",
           "Race Not Available",
           "American Indian or Alaska Native",
           "Black or African American",
           "2 or more minority races",
           "White",
           "Joint") %>% lapply(URLencode)

cfpb_api_gofer <- function(url){
  resp <- GET(url)
  result <- fromJSON(content(resp, "text", encoding="UTF-8"),                                      # No json/xml option for this API
                     simplifyDataFrame=TRUE) %>% purrr::pluck("aggregations")
  return(result)
}

cfpb_url_builder <- function(dyear, actions_taken, races=NULL, ethnicities=NULL,
                             sexes=NULL, dwelling_categories=NULL){
  url <- paste0("https://ffiec.cfpb.gov/v2/data-browser-api/view/aggregations?states=WA",
                paste0(c("42644","14740","45104"), collapse=","), "&years=", dyear,
                "&actions_taken=", actions_taken)
  if(!is.null(dwelling_categories)){
    url %<>% paste0("&dwelling_categories=", paste0(races, collapse=","))}
  if(!is.null(ethnicities)){
    url %<>% paste0("&ethnicities=", paste0(races, collapse=","))}
  if(!is.null(races)){
    url %<>% paste0("&races=", paste0(races, collapse=","))}
  if(!is.null(sexes)){
    url %<>% paste0("&sexes=", paste0(races, collapse=","))}
  return(url)
}

mixr <- expand.grid(dyear=2018:2021, actions_taken=1:6, race=races)                                 # Notice the API aggregates all specified items
ls1 <- list()
ls1 <- mapply(cfpb_url_builder, mixr$actions_taken, mixr$race) %>%                                  # So years, details must be called individually
  lapply(cfpb_api_gofer) %>% rbindlist()


#url <- paste0("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv?counties=",counties,"&years=2018")
