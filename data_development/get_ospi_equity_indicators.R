library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(RSocrata)

# Setup: List necessary variables and lookups -----------------------

counties        <- c("King","Kitsap","Pierce","Snohomish")
reported_groups <- c("Non-Low Income", "Non-English Language Learners", "Students without Disabilities", "White")
target_groups   <- c("Low Income", "English Language Learners", "Students with Disabilities", "POC")
focus_types     <- c("Income_cat","LEP_cat","Disability_cat","POC_cat")
levels_order    <- rbind(target_groups, reported_groups) %>% unlist() %>% c("All Students") %>% recode("White"="Non-POC")
swap_label      <- data.frame(target_groups=target_groups,
                              focus_attribute=recode(reported_groups,"White"="Non-POC"),
                              focus_type=focus_types) %>% setDT()
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

# Create OSPI to PSRC label lookup
label_lookup <- data.frame(
  chg_ospi_labels=t(swap_label[2:3,1:2]) %>% unlist %>% c("All Students"),
  to_psrc_labels=c("Limited English proficiency","English proficient","With disability","Without disability","Total")
  ) %>% setDT()

read.wa.Socrata <- function(URL){
  x <- read.socrata(url=URL, app_token=Sys.getenv("DATAWAGOV_APPTOKEN"), email=Sys.getenv("MYEMAIL"), password=Sys.getenv("DATAWAGOV_CRED"))
  return(x)
}

# Elmer interaction -------------------------------------------------
ospi_efa_to_elmer <- function(rs_kready){
  merge_sql <- paste("MERGE INTO equity.indicator_facts WITH (HOLDLOCK) AS target",                # This SQL updates existing values and/or inserts any new ones
                     "USING stg.equity_ospi AS source",
                     "ON target.data_year=source.data_year AND ",
                     "target.span=1 AND ",
                     "target.county=source.county AND",
                     "target.focus_type=source.focus_type AND",
                     "target.focus_attribute=source.focus_attribute AND",
                     "target.indicator_type='Kindergarten readiness' AND",
                     "target.indicator_attribute='6 for 6 dimensions'",
                     "WHEN MATCHED THEN UPDATE SET target.fact_value=source.fact_value,",
                     "target.margin_of_error=0",
                     "WHEN NOT MATCHED BY TARGET THEN INSERT ",
                     "(data_year, span, county, focus_type, focus_attribute, indicator_type,",
                     "indicator_attribute, fact_type, fact_value, margin_of_error)",
                     "VALUES (source.data_year, 1, source.county, source.focus_type,",
                     "source.focus_attribute, 'Kindergarten readiness', '6 for 6 dimensions',",
                     "'share', source.fact_value, 0);")

  psrcelmer::stage_table(rs_kready, "equity_ospi")                                                 # Stage table first
  psrcelmer::sql_execute(sql=merge_sql)                                                            # -- then merge
  psrcelmer::sql_execute(sql="DROP TABLE stg.equity_ospi")                                         # Clean up
  return(invisible(NULL))                                                                          # (Return object unapplicable)
}


# Main function -----------------------------------------------------

get_k_readiness <- function(URL){
  reported <- list()
  reported[[1]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Northwest%20Educational%20Service%20District%20189"))
  reported[[2]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Puget%20Sound%20Educational%20Service%20District%20121"))
  reported[[3]] <- read.wa.Socrata(paste0(URL,"?organizationlevel=District&esdname=Olympic%20Educational%20Service%20District%20114"))
  reported %<>% rbindlist(use.names=TRUE) %>% .[, c("numerator","denominator"):=lapply(.SD, as.integer), .SDcols=c("numerator","denominator")] %>%
    .[, County:=fcase(grepl("121$", esdname) & grepl(county_lookup$King, districtname),     "King",
                      grepl("114$", esdname) & grepl(county_lookup$Kitsap, districtname),   "Kitsap",
                      grepl("121$", esdname) & grepl(county_lookup$Pierce, districtname),   "Pierce",
                      grepl("189$", esdname) & grepl(county_lookup$Snohomish, districtname),"Snohomish")] %>%               # Create County field
    .[!is.na(County)] %>% .[measure=="NumberofDomainsReadyforKindergarten" & measurevalue=="6"] %>%                         # Filter to specific 6-for-6 dimensions indicator
    .[, c("esdname", "organizationlevel"):=NULL] %>%
  setnames(c("studentgrouptype","studentgroup"),c("focus_type","focus_attribute"))
  reported[focus_attribute=="White", focus_attribute:="Non-POC"]
  reported %<>% .[, focus_type:=fcase(focus_type=="FederalRaceEthnicity","POC_cat",
                                      focus_type=="SpecialEd","Disability_cat",
                                      focus_type=="Bilingual","LEP_cat",
                                      focus_type=="FreeLunch","Income_cat")]

  reference <- reported[focus_attribute=="All Students"] %>% .[!is.na(numerator)]                                           # Totals into separate table
  reported %<>% .[focus_attribute %in% swap_label$focus_attribute]
  combined <- copy(reported) %>% .[swap_label, `:=`(focus_attribute=target_groups, focus_type=focus_type), on=.(focus_attribute)] %>%
    .[reference, `:=`(numerator=as.double(i.numerator-numerator),                                                           # EFA as remainder to dominant groups
                      denominator=as.double(i.denominator-denominator)), on=.(districtname)] %>%
    rbind(reported) %>% .[denominator!=0 & !is.na(numerator)] %>%                                                           # Combine w/ non-EFA (for comparison)
    .[, `:=`(focus_attribute=factor(focus_attribute, levels=levels_order),                                                  # Factors for custom ordering
             County=factor(County, levels=c(unlist(counties),"Region")))]

  rs <- list()
  rs[[1]] <- combined[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","County","focus_attribute","focus_type")] # EFA - County
  rs[[2]] <- combined[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","focus_attribute","focus_type")]          # EFA - Region
  rs[[2]][, County:="Region"]
  rs[[3]] <- reference[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","County","focus_attribute","focus_type")] # All students - County
  rs[[4]] <- reference[, lapply(.SD, sum), .SDcols=c("numerator","denominator"), by=c("schoolyear","focus_attribute","focus_type")]          # All students - Region
  rs[[4]][, County:="Region"]
  rs %<>% rbindlist(use.names=TRUE)
  rs[, `:=`(indicator_type="Kindergarten readiness", fact_value=as.double(numerator)/denominator)] %>%
    .[, c("numerator", "denominator"):=NULL] %>%                                                                            # Share as ratio of reported district totals
    setorder(schoolyear, County, focus_attribute)
  rs[focus_attribute=="All Students", focus_type:="Total"]
  rs[label_lookup, focus_attribute:=to_psrc_labels, on=.(focus_attribute=chg_ospi_labels)]
  rs[, data_year:=as.integer(str_sub(schoolyear,1L,4L))]                                                                    # Schoolyear start - integer format in Elmer
  rs[, schoolyear:=NULL]
  return(rs)
}

# Example -----------------------------------------------------------
## Single year
# url <- "https://data.wa.gov/resource/vumg-9sgs.json"
# kready <- get_k_readiness(url)

# All years
# urlvector <- c("https://data.wa.gov/resource/vumg-9sgs.json",  # 2023-24
#                "https://data.wa.gov/resource/3ji8-ykgj.json",  # 2022-23
#                "https://data.wa.gov/resource/rzgf-vi75.json",  # 2021-22
#                                                                # 2020-21 Not reported/collected
#                "https://data.wa.gov/resource/26rj-f9wn.json",  # 2019-20
#                "https://data.wa.gov/resource/p4sv-js2m.json",  # 2018-19
#                "https://data.wa.gov/resource/huwq-t84x.json",  # 2017-18
#                "https://data.wa.gov/resource/2x4x-bzqs.json",  # 2016-17
#                "https://data.wa.gov/resource/8ewp-xtgm.json",  # 2015-16
#                "https://data.wa.gov/resource/yaag-7vv4.json",  # 2014-15
#                "https://data.wa.gov/resource/rjgh-459t.json",  # 2013-14
#                "https://data.wa.gov/resource/sedr-qag9.json",  # 2012-13
#                "https://data.wa.gov/resource/59cw-kpf6.json")  # 2011-12 first year available via API)
# kready <- list()
# kready <- lapply(urlvector, get_k_readiness) %>% rbindlist(use.names=TRUE)
# ospi_efa_to_elmer(kready)
