library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(data.table)

# 1. Setup: List necessary direct-PUMS variables --------------------

# PUMS variables desired at person level
pvars <- c("AGEP",                   # used as condition >25yo
           "DIS",                    # Disability - unavailable before 2012
           "LNGI",                   # Limited English speaking household
           "PRACE",                  # Individual race (PSRC categories)
           "POVPIP",                 # Income-to-poverty ratio (all household members share it)
           "PRIVCOV",                # Private health insurance coverage
           "PUBCOV",                 # Public health insurance coverage
           "R18",                    # Presence of persons under 18 years in household
           "R65",                    # Presence of persons over 65 years in household
           "SCHL"                    # Educational attainment
           )

# PUMS variables desired at household level
hvars <- c(
           "ACCESSINET",             # Internet access; "ACCESS" from 2017 5yr to 2019 5yr
           "RMSP",                   # Number of rooms
           "FS",                     # Supplemental Nutrition Assistance Program
           "GRPIP",                  # Gross rent as percentage of income
           "GRNTP",                  # Gross rent
           "HDIS",                   # Any member of household disabled - unavailable before 2012
           "HINCP",                  # Household income
           "HRACE",                  # Household race (PSRC categories)
           "LNGI",                   # Limited English speaking household
           "NP",                     # Number of persons in household
           "OCPIP",                  # Homeowner costs as percentage of income
           "OWN_RENT",               # Simplified housing tenure (PSRC categories)
           "POVPIP",                 # Income-to-poverty ratio
           "PRACE",                  # Householder race (PSRC categories)
           "SMOCP",                  # Homeowner costs
           "R18",                    # Presence of persons under 18 years in household
           "R65",                    # Presence of persons over 65 years in household
           "TEN"                     # Housing tenure
           )

# 2. Setup: List and define Equity Focus Area (EFA) variables -------

which_efa_vars <- function(dyear){
  efa_vars <- c("POC_cat", "Income_cat", "Disability_cat", "Youth_cat", "Older_cat", "LEP_cat")
  efa_vars %<>% .[! efa_vars %in% "Disability_cat"]
  return(efa_vars)
}

# Create the EFA variables from available PUMS variables
add_efa_vars <- function(so){
  dyear <- so[[7]]$DATA_YEAR %>% unique
  so_plus <- mutate(so,
    POC_cat     = factor(case_when(PRACE=="White alone"          ~ "Non-POC",
                            !is.na(PRACE)                        ~ "POC"),
                         levels=c("POC","Non-POC")),
    Income_cat  = factor(case_when(POVPIP<200                    ~ "Low Income",
                            !is.na(POVPIP)                       ~ "Non-Low Income")),
    Youth_cat   = factor(case_when(grepl("^No ", R18)            ~ "Household without youth",
                            !is.na(R18)                          ~ "Household with youth")),
    Older_cat   = factor(case_when(grepl("^No ", R65)            ~ "Household without older adult",
                            !is.na(R65)                          ~ "Household with older adult")),
    LEP_cat     = factor(case_when(grepl("^No one", LNGI)        ~ "Limited English proficiency",
                            !is.na(LNGI)                         ~ "English proficient")))
  if(dyear > 2011){
    so_plus <- mutate(so_plus,
      Disability_cat=if("DIS" %in% colnames(so)){
        factor(case_when(grepl("^With ", DIS)     ~ "With disability",
                         grepl("^Without ", DIS)  ~ "Without disability"))
      }else if("HDIS" %in% colnames(so)){
        factor(case_when(grepl("^With ", HDIS)    ~ "With disability",
                         grepl("^Without ", HDIS) ~ "Without disability"))
      })
  }
  return(so_plus)
}

# 3. Setup: Helper functions ----------------------------------------

# These two bulk stat functions give results for a single subject variable sliced by each of the EFA categories,
#  -- at both county and regional levels.

# Specific to counts--subject variable is a categorical variable (i.e. group_var)
bulk_count_efa <- function(so, analysis_var){
  efa_vars <- which_efa_vars(unique(so[[7]]$DATA_YEAR))
  reglist <- efa_vars %>% lapply(FUN=function(x) c(x, analysis_var))
  ctylist <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY", analysis_var))
  rs      <- list()
  rs[[1]] <- pums_bulk_stat(so, "count", group_var_list=reglist, incl_na=FALSE)                    # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- pums_bulk_stat(so, "count", group_var_list=ctylist, incl_na=FALSE) %>%
                .[COUNTY!="Region"]                                                                # Remove duplicate total level
  rs %<>% rbindlist() %>% .[, indicator_type:=analysis_var] %>%                                    # Combine county & regional results
    setnames(analysis_var, "indicator_attribute") %>%
    arrange(DATA_YEAR, var_name, COUNTY)
  return(rs)
}

# For sums, medians or means--statistic is summarizing the subject variable (i.e. stat_var)
bulk_stat_efa <- function(so, stat_type, analysis_var){
  efa_vars <- which_efa_vars(unique(so[[7]]$DATA_YEAR))
  ctylist  <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY"))
  rs       <- list()
  rs[[1]]  <- pums_bulk_stat(so, stat_type, analysis_var, efa_vars, incl_na=FALSE)                 # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]]  <- pums_bulk_stat(so, stat_type, analysis_var, ctylist, incl_na=FALSE) %>%
                 .[COUNTY!="Region"]                                                               # Remove duplicate total level
  rs %<>% rbindlist() %>% .[, `:=`(indicator_type=analysis_var, indicator_attribute="N/A")] %>%    # Combine county & regional results
    arrange(DATA_YEAR, var_name, COUNTY)
  return(rs)
}

format_for_elmer <- function(x,y){
  dt <- setDT(x)
  if(TRUE %in% grepl("median",colnames(dt))){                                                       # Either median or share (count is dropped)
    dt[,`:=`(fact_type="median")]
  }else if(TRUE %in% grepl("count",colnames(dt))){
    dt[, fact_type:="share of population"]
    dt[, grep("^count$|^count_moe$", colnames(dt)):=NULL]
  }
  setnames(dt, c("var_name","var_value"), c("focus_type","focus_attribute"))
  setnames(dt, grep("share$|median$", colnames(dt)), c("fact_value"))
  setnames(dt, grep("_moe$", colnames(dt)), c("margin_of_error"))
  setnames(dt, colnames(dt), tolower(colnames(dt)))
  dt[, `:=`(indicator_type=y, span=5)]
  return(dt)
}

efa_to_elmer <- function(efa_result){
  merge_sql <- paste("MERGE INTO equity.indicator_facts WITH (HOLDLOCK) AS target",                # This SQL updates existing values and/or inserts any new ones
                     "USING stg.equity_pums AS source",
                     "ON target.data_year=source.data_year AND",
                     "target.span=source.span AND",
                     "target.county=source.county AND",
                     "target.focus_type=source.focus_type AND",
                     "target.focus_attribute=source.focus_attribute AND",
                     "target.indicator_type=source.indicator_type AND",
                     "target.indicator_attribute=source.indicator_attribute",
                     "WHEN MATCHED THEN UPDATE SET target.fact_value=source.fact_value,",
                     "target.margin_of_error=source.margin_of_error",
                     "WHEN NOT MATCHED BY TARGET THEN INSERT ",
                     "(data_year, span, county, focus_type, focus_attribute, indicator_type,",
                     "indicator_attribute, fact_type, fact_value, margin_of_error)",
                     "VALUES (source.data_year, source.span, source.county, source.focus_type,",
                     "source.focus_attribute, source.indicator_type, source.indicator_attribute,",
                     "source.fact_type, source.fact_value, source.margin_of_error);")

  efa_result %<>% mapply(format_for_elmer, ., as.character(names(.)), USE.NAMES=TRUE, SIMPLIFY=FALSE) # Rename fields/reshape to fit Elmer.equity schema
  efa_result %<>% rbindlist(use.names=TRUE)                                                       # Combine to one data.table
  psrcelmer::stage_table(efa_result, "equity_pums")
  psrcelmer::sql_execute(sql=merge_sql)                                                           # Execute the query
  psrcelmer::sql_execute(sql="DROP TABLE stg.equity_pums")                                        # Clean up
  return(invisible(NULL))
}

# 4. Main functions -------------------------------------------------

# Subject variables are defined within the primary function
#  -- but this could be separated into additional setup functions, if preferred

# Generate all indicators for a single survey
pums_efa_singleyear <- function(dyear){
  refyear <- 2021  # Dollar year for inflation-adjusted comparisons
  pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds" # Network PUMS location

  if(dyear < 2012){hvars <- replace(hvars, hvars=="RMSP","RMS")}                                   # Variable changed names w/ 2012 data
  if(dyear %in% 2017:2020){hvars <- replace(hvars, hvars=="ACCESSINET","ACCESS")                   # Variable changed names w/ 2020 data
  }else if(dyear<2017){hvars %<>% .[! hvars %in% "ACCESSINET"]}
  if(dyear < 2012){
    pvars %<>% .[! pvars %in% c("DIS", "PRIVCOV", "PUBCOV")]
    hvars %<>% .[! hvars %in% "HDIS"]
  }

  pp_df <- get_psrc_pums(span=5, dyear, "p", pvars, dir=pums_rds)                                  # Retrieve persons data
  pp_df %<>% add_efa_vars() %>% mutate(
               edu_simp = factor(case_when(AGEP<25                       ~ NA_character_,          # Define the educational attainment subject variable
                  grepl("(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",
                  !is.na(SCHL)                         ~ "Less than a Bachelor's degree")))
  if(dyear > 2011){
    pp_df %<>% mutate(
               healthcov = factor(case_when(AGEP<25                      ~ NA_character_,          # Define the health insurance coverage subject variable
                  grepl("^With ", PRIVCOV)|grepl("^With ", PUBCOV) ~ "With health insurance",
                  grepl("^Without ", PRIVCOV) & grepl("^Without ", PUBCOV) ~ "Without health insurance")))
  }

  hh_df <- get_psrc_pums(span=5, dyear, "h", hvars, dir=pums_rds)                                  # Retrieve household data
  if("ACCESS" %in% colnames(hh_df)){hh_df %<>% rename("ACCESSINET"="ACCESS")}                      # Variable changed names w/ 2020 data
  if("RMS" %in% colnames(hh_df)){
    hh_df %<>% mutate(RMS=as.integer(stringr::str_extract(as.character(RMS),"^\\d+")))             # Convert from factor to value
    hh_df %<>% rename("RMSP"="RMS")                                                                # Variable changed names w/ 2012 data
    }
  hh_df %<>% real_dollars(refyear) %>% add_efa_vars() %>% mutate(
               poverty=Income_cat,                                                                 # Identical to Income_cat
               housing_burden=factor(case_when(                                                    # Define the housing cost burden subject variable
                                     GRPIP<30|OCPIP<30|SMOCP==0|(is.na(GRNTP) & is.na(SMOCP)) ~ "Less than 30 percent",
                                     dplyr::between(GRPIP,30,50)|dplyr::between(OCPIP,30,50) ~ "Between 30 and 50 percent",
                                     GRPIP>50|OCPIP>50|is.na(HINCP)      ~ "Greater than 50 percent"),
                                levels=c("Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent")),
               rent_burden=factor(case_when(                                                       # Define the rent burden subject variable
                                         GRPIP<30|(is.na(GRNTP) & OWN_RENT=="Rented") ~ "Less than 30 percent",
                                         dplyr::between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                                         GRPIP>50|is.na(HINCP)       ~ "Greater than 50 percent"),
                                    levels=c("Greater than 50 percent",
                                             "Between 30 and 50 percent",
                                             "Less than 30 percent")),
               renter_crowding = factor(case_when(is.na(NP)|is.na(RMSP)|RMSP==0 ~ NA_character_,          # Define the crowding subject variable
                                           NP/RMSP <= 1   & OWN_RENT=="Rented" ~ "One person per room or less",
                                           NP/RMSP  > 1 & OWN_RENT=="Rented" ~ "More than 1 person per room"),
                                 levels=c("More than 1 person per room",
                                          "One person per room or less")),
               owner_crowding = factor(case_when(is.na(NP)|is.na(RMSP)|RMSP==0 ~ NA_character_,          # Define the crowding subject variable
                                           NP/RMSP <= 1   & OWN_RENT=="Owned" ~ "One person per room or less",
                                           NP/RMSP  > 1 & OWN_RENT=="Owned" ~ "More than 1 person per room"),
                                 levels=c("More than 1 person per room",
                                          "One person per room or less")),
               renter_median_hh_income = case_when(OWN_RENT=="Rented" ~ !!as.name(paste0("HINCP",refyear)),
                                                   TRUE ~ NA_real_)
  )
  if(dyear > 2016){
    hh_df %<>% mutate(
               internet = factor(case_when(grepl("^Yes", ACCESSINET) ~ "With internet access", # Define the internet access subject variable; began in 2013
                                        grepl("^No", ACCESSINET)     ~ "Without internet access")))
  }

  deep_pocket <- list()
  deep_pocket$"educational_attainment"  <- bulk_count_efa(pp_df, "edu_simp")
  if(dyear > 2011){
    deep_pocket$"healthcare_coverage"     <- bulk_count_efa(pp_df, "healthcov")
  }
  deep_pocket$"median_household_income" <- bulk_stat_efa(hh_df, "median", paste0("HINCP",refyear)) # Notice dollar comparisons require inflation adjustment
  deep_pocket$"household_poverty"       <- bulk_count_efa(hh_df, "poverty")
  deep_pocket$"housing_cost_burden"     <- bulk_count_efa(hh_df, "housing_burden")
  deep_pocket$"tenure"                  <- bulk_count_efa(hh_df, "OWN_RENT")
  deep_pocket$"rent_burden"             <- bulk_count_efa(hh_df, "rent_burden")
  deep_pocket$"median_gross_rent"       <- bulk_stat_efa(hh_df, "median", paste0("GRNTP",refyear))
  deep_pocket$"renter_crowding"         <- bulk_count_efa(hh_df, "renter_crowding")                # i.e. persons per room for renters
  deep_pocket$"owner_crowding"          <- bulk_count_efa(hh_df, "owner_crowding")                 # i.e. persons per room for owners
  deep_pocket$"SNAP"                    <- bulk_count_efa(hh_df, "FS")                             # food stamp/SNAP
  if(dyear > 2016){
    deep_pocket$"internet_access"         <- bulk_count_efa(hh_df, "internet")
  }
  deep_pocket$"renter_median_hh_income" <- bulk_stat_efa(hh_df, "median", "renter_median_hh_income")

  return(deep_pocket)
}

# Generate trend data, i.e. all indicators across multiple years; returns a list                   # Problematic if variables aren't present in all requested years
pums_efa_multiyear <- function(dyears){                                                            # -- verify in PUMS data dictionaries for each survey
  refyear <- max(dyears)
  rs_master <- lapply(dyears, pums_efa_singleyear) %>%
    as.data.frame(do.call(rbind, lapply(., as.vector)))                                            # Combine matching indicator tables across years
  return(rs_master)                                                                                # Return the object
}

# Write all tables to file
write_pums_efa <- function(efa_rs_list){
  mapply(fwrite, efa_rs_list, paste(names(efa_rs_list), ".csv", sep=""))
  return(NULL)
}

# Example 1: Generate indicators for a single year/span -------------
# equity_2022_5 <- pums_efa_singleyear(2022)                                                       # Returns all tables as separate items in a list
# write_pums_efa(equity_2022_5)                                                                    # Write the tables to .csv
# efa_to_elmer(equity_2022_5)
