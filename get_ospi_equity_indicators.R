library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(RSocrata)

# Setup: List necessary variables and lookups -----------------------

counties        <- c("King","Kitsap","Pierce","Snohomish")
reported_groups <- c("Non-Low Income", "Non-English Language Learners", "Students without Disabilities", "White")
target_groups   <- c("Low Income", "English Language Learners", "Students with Disabilities", "POC")
levels_order    <- rbind(target_groups, reported_groups) %>% unlist() %>% c("All Students") %>% recode("White"="Non-POC")
swap_label      <- data.frame(target_groups=target_groups, studentgroup=recode(reported_groups,"White"="Non-POC")) %>% setDT()
keepcols        <- c("schoolyear", "esdname", "districtname", "OrganizationLevel",
                     "studentgroup", "Domain", "MeasureValue", "numerator", "denominator")

# Create district-to-county regex lookup
county_lookup <- vector(mode='list', length=4)
names(county_lookup) <- counties
county_lookup[[1]] <- paste("Auburn", "Bellevue", "Enumclaw", "Federal Way", "Highline",
                            "Issaquah", "Kent ", "Lake Washington", "Mercer Island", "Northshore",
                            "Renton", "Riverview", "Seattle", "Shoreline", "Skykomish",
                            "Snoqualmie Valley", "Tahoma", "Tukwila", "Vashon Island", sep="|")
county_lookup[[2]] <- paste("Bainbridge Island", "Central Kitsap", "North Kitsap", "South Kitsap", sep="|")
county_lookup[[3]] <- paste("Bethel", "Carbonado", "Clover Park", "Dieringer", "Eatonville", "Fife",
                            "Franklin Pierce", "Orting", "Peninsula", "Puyallup", "Steilacoom West",
                            "Sumner", "Tacoma", "University Place", "White River", sep="|")
county_lookup[[4]] <- paste("Arlington","Darrington", "Edmonds", "Everett", "Granite Falls",
                            "Index", "Lake Stevens", "Lakewood","Marysville", "Monroe",
                            "Mukilteo", "Snohomish", "Stanwood", "Sultan", sep="|")

read.wa.Socrata <- function(URL){
  x <- read.socrata(url=URL, app_token=Sys.getenv("DATAWAGOV_APPTOKEN"), email=Sys.getenv("MYEMAIL"), password=Sys.getenv("DATAWAGOV_CRED"))
  return(x)
}

# Main function -----------------------------------------------------

get_k_readiness <- function(URL){
  reported <- list()
  reported[[1]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Northwest%20Educational%20Service%20District%20189"))
  reported[[2]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Puget%20Sound%20Educational%20Service%20District%20121"))
  reported[[3]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Olympic%20Educational%20Service%20District%20114"))
  reported %<>% rbindlist() %>% .[, c("numerator","denominator"):=lapply(.SD, as.integer), .SDcols=c("numerator","denominator")] %>%
    .[, County:=fcase(grepl("121$", esdname) & grepl(county_lookup$King, districtname),     "King",
                      grepl("114$", esdname) & grepl(county_lookup$Kitsap, districtname),   "Kitsap",
                      grepl("121$", esdname) & grepl(county_lookup$Pierce, districtname),   "Pierce",
                      grepl("189$", esdname) & grepl(county_lookup$Snohomish, districtname),"Snohomish")] %>%                 # Create County field
    .[!is.na(County)] %>% .[measure=="NumberofDomainsReadyforKindergarten" & measurevalue=="6"] %>%                         # Filter to specific 6-for-6 dimensions indicator
    .[, c("esdname", "organizationlevel"):=NULL]
  reported[studentgroup=="White", studentgroup:="Non-POC"]

  reference <- reported[studentgroup=="All Students"] %>% .[!is.na(numerator)]                                              # Totals into separate table
  reported %<>% .[studentgroup %in% swap_label$studentgroup]
  combined <- copy(reported) %>% .[swap_label, studentgroup:=target_groups, on=.(studentgroup)] %>%
    .[reference, `:=`(numerator=as.double(i.numerator-numerator),                                                # EFA as remainder to dominant groups
                      denominator=as.double(i.denominator-denominator)), on=.(districtname)] %>%
    rbind(reported) %>% .[denominator!=0 & !is.na(numerator)] %>%                                                           # Combine w/ non-EFA (for comparison)
    .[, `:=`(studentgroup=factor(studentgroup, levels=levels_order),                                                        # Factors for custom ordering
             County=factor(County, levels=c(unlist(counties),"Region")))]

  rs <- list()
  rs[[1]] <- combined[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","County","studentgroup")] # EFA - County
  rs[[2]] <- combined[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","studentgroup")]          # EFA - Region
  rs[[2]][, County:="Region"]
  rs[[3]] <- reference[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","County","studentgroup")] # All students - County
  rs[[4]] <- reference[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","studentgroup")]          # All students - Region
  rs[[4]][, County:="Region"]
  rs %<>% rbindlist(use.names=TRUE)
  rs[, KReadiness:=as.double(numerator)/denominator] %>% .[, c("numerator", "denominator"):=NULL] %>%                       # Share as ratio of reported district totals
    setorder(schoolyear, County, studentgroup)
  return(rs)
}

# Example -----------------------------------------------------------
## Single year
# url <- "https://data.wa.gov/resource/rzgf-vi75.json"
# x <- get_k_readiness(url)

## All years
# urlvector <- c("https://data.wa.gov/resource/rzgf-vi75.json",
#                "https://data.wa.gov/resource/26rj-f9wn.json",
#                "https://data.wa.gov/resource/p4sv-js2m.json",
#                "https://data.wa.gov/resource/huwq-t84x.json",
#                "https://data.wa.gov/resource/2x4x-bzqs.json",
#                "https://data.wa.gov/resource/8ewp-xtgm.json",
#                "https://data.wa.gov/resource/yaag-7vv4.json",
#                "https://data.wa.gov/resource/rjgh-459t.json",
#                "https://data.wa.gov/resource/sedr-qag9.json",
#                "https://data.wa.gov/resource/59cw-kpf6.json")
# allyrs <- list()
# allyrs <- lapply(get_k_readiness) %>% rbindlist()
