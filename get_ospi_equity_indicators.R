library(magrittr)
library(dplyr)
library(stringr)
library(data.table)

# Setup: List necessary variables and lookups -----------------------

counties        <- c("King","Kitsap","Pierce","Snohomish")
reported_groups <- c("Non-Low Income", "Non-English Language Learners", "Students without Disabilities", "White")
target_groups   <- c("Low Income", "English Language Learners", "Students with Disabilities", "POC")
levels_order    <- rbind(target_groups, reported_groups) %>% recode("White"="Non-POC")
swap_label      <- data.frame(target_groups=target_groups, StudentGroup=recode(reported_groups,"White"="Non-POC")) %>% setDT()
keepcols        <- c("schoolyear", "ESDName", "DistrictName", "OrganizationLevel",
                     "StudentGroup", "Domain", "MeasureValue", "Numerator", "Denominator")

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

# Main function -----------------------------------------------------

get_k_readiness <- function(path){
  reported <- fread(path, select=keepcols)
  reported %<>% .[grepl("114$|121$|189$", ESDName) & OrganizationLevel=="District" &                                        # Filter geography & level
                  StudentGroup %in% c("All Students", unlist(reported_groups)) & MeasureValue=="Kindergarten and up" &      # Limit to kindergarten-ready only
                  Domain!="NumberofDomains" & !is.na(Denominator)]

  reported[, County:=fcase(grepl("121$", ESDName) & grepl(county_lookup$King, DistrictName),     "King",
                           grepl("114$", ESDName) & grepl(county_lookup$Kitsap, DistrictName),   "Kitsap",
                           grepl("121$", ESDName) & grepl(county_lookup$Pierce, DistrictName),   "Pierce",
                           grepl("189$", ESDName) & grepl(county_lookup$Snohomish, DistrictName),"Snohomish")]              # Create County field
  reported %<>% .[!is.na(County)] %>% .[, c("ESDName", "OrganizationLevel","MeasureValue"):=NULL]
  reported[StudentGroup=="White", StudentGroup:="Non-POC"]

  reference <- reported[StudentGroup=="All Students"]                                                                       # Totals into separate table
  reported %<>% .[StudentGroup!="All Students"]
  combined <- copy(reported) %>% .[swap_label, StudentGroup:=target_groups, on=.(StudentGroup)] %>%
               .[reference, `:=`(Numerator=as.double(i.Numerator-Numerator),                                                # EFA as remainder to dominant groups
                                 Denominator=as.double(i.Denominator-Denominator)), on=.(DistrictName, Domain)] %>%
               .[Denominator!=0] %>% rbind(reported)                                                                        # Combine w/ non-EFA (for comparison)
  combined[, `:=`(StudentGroup=factor(StudentGroup, levels=levels_order),                                                   # Factors for custom ordering
                  County=factor(County, levels=c(unlist(counties),"Region")))]

  rs <- list()
  rs[[1]] <- combined[, lapply(.SD, sum), .SDcols=c("Numerator","Denominator"), by=c("schoolyear","County","StudentGroup")] # County
  rs[[2]] <- combined[, lapply(.SD, sum), .SDcols=c("Numerator","Denominator"), by=c("schoolyear","StudentGroup")]          # Region
  rs[[2]][, County:="Region"]
  rs %<>% rbindlist(use.names=TRUE)
  rs[, KReadiness:=as.double(Numerator)/Denominator] %>% .[, c("Numerator", "Denominator"):=NULL] %>%                       # Share as total ready dimensions across students x dimensions
    setorder(schoolyear, County, StudentGroup)
  return(rs)
}

# Example -----------------------------------------------------------
# path <- "data/Report_Card_WaKids_2021-22_School_Year.csv"                                                                 # Figure out OSPI API later
# x <- get_k_readiness(path)

