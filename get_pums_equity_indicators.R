library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(data.table)

# 1. Setup: List necessary direct-PUMS variables --------------------

# PUMS variables desired at person level
pvars <- c("AGEP",                   # used as condition >25yo
           "DIS",                    # Disability
           "LNGI",                   # Limited English speaking household
           "PRACE",                  # Individual race (PSRC categories)
           "POVPIP",                 # Income-to-poverty ratio (all household members share it)
           "PRIVCOV",                # Private health insurance coverage
           "PUBCOV",                 # Public health insurance coverage
#           "R18",                    # Presence of persons under 18 years in household
#           "R65",                    # Presence of persons over 65 years in household
           "SCHL")                   # Educational attainment

# PUMS variables desired at household level
hvars <- c("ACCESSINET",             # Internet access
           "BDSP",                   # Number of bedrooms
           "FS",                     # Supplemental Nutrition Assistance Program
           "GRPIP",                  # Gross rent as percentage of income
           "GRNTP",                  # Gross rent
           "HDIS",                   # Any member of household disabled
           "HINCP",                  # Household income
           "HRACE",                  # Household race (PSRC categories)
           "LNGI",                   # Limited English speaking household
           "NP",                     # Number of persons in household
           "OCPIP",                  # Homeowner costs as percentage of income
           "POVPIP",                 # Income-to-poverty ratio
           "PRACE",                  # Householder race (PSRC categories)
           "SMOCP",                  # Homeowner costs
#           "R18",                    # Presence of persons under 18 years in household
#           "R65",                    # Presence of persons over 65 years in household
           "TEN")                    # Housing tenure

# 2. Setup: List and define Equity Focus Area (EFA) variables -------

efa_vars <- c("POC_cat", "Income_cat", "Disability_cat", "LEP_cat") # "Youth_cat", "Older_cat",

# Create the EFA variables from available PUMS variables
add_efa_vars <- function(so){
  so_plus <- mutate(so,
    POC_cat     = factor(case_when(PRACE=="White alone"          ~ "Non-POC",
                            !is.na(PRACE)                        ~ "POC"),
                         levels=c("POC","Non-POC")),
    Income_cat  = factor(case_when(POVPIP<200                    ~ "Low Income",
                            !is.na(POVPIP)                       ~ "Non-Low Income")),
    Disability_cat=if("DIS" %in% colnames(so)){
                       factor(case_when(grepl("^With ", DIS)     ~ "Disability",
                                 grepl("^Without ", DIS)         ~ "No disability"))
                   }else if("HDIS" %in% colnames(so)){
                       factor(case_when(grepl("^With ", HDIS)    ~ "Disability",
                                 grepl("^Without ", HDIS)        ~ "No disability"))
                   },
    # Youth_cat   = factor(case_when(grepl("^1 or more", R18)      ~ "Household with youth",
    #                         !is.na(R18)                          ~ "Household without youth"),
    #                      levels=c("Youth","Non-youth")),
    # Older_cat   = factor(case_when(grepl("^1 or|^2 or", R65)     ~ "Household with older adult",
    #                         !is.na(R65)                          ~ "Household without older adult"),
    #                      levels=c("Older adult","Non-older adult")),
    LEP_cat     = factor(case_when(grepl("^No one", LNGI)        ~ "Limited English proficiency",
                            !is.na(LNGI)                         ~ "English proficient")))
  return(so_plus)
}

# 3. Setup: Helper functions ----------------------------------------

# These two bulk stat functions give results for a single subject variable sliced by each of the EFA categories,
#  -- at both county and regional levels.

# Specific to counts--subject variable is a categorical variable (i.e. group_var)
bulk_count_efa <- function(so, anal_var){
  reglist <- efa_vars %>% lapply(FUN=function(x) c(x, anal_var))
  ctylist <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY", anal_var))
  rs      <- list()
  rs[[1]] <- pums_bulk_stat(so, "count", group_var_list=reglist, incl_na=FALSE)                    # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- pums_bulk_stat(so, "count", group_var_list=ctylist, incl_na=FALSE) %>%
                .[COUNTY!="Region"]                                                                # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, var_name, COUNTY)                                     # Combine county & regional results
  return(rs)
}

# For sums, medians or means--statistic is summarizing the subject variable (i.e. stat_var)
bulk_stat_efa <- function(so, stat_type, anal_var){
  ctylist  <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY"))
  rs       <- list()
  rs[[1]]  <- pums_bulk_stat(so, stat_type, anal_var, efa_vars, incl_na=FALSE)                     # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]]  <- pums_bulk_stat(so, stat_type, anal_var, ctylist, incl_na=FALSE) %>%
                 .[COUNTY!="Region"]                                                               # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, var_name, COUNTY)                                     # Combine county & regional results
  return(rs)
}

# Inflation adjustment to compare dollar amounts between separate surveys (nominal 2019 Dollars)
# -- Within-survey inflation adjustment is already handled in the package
deflate_2019 <- function(df){
  lookup <- data.table(c(2000:2021),
                       c(1.407632219, 1.379958823, 1.362072811, 1.334037234, 1.301730161,          # Annual PCE deflator from FRED
                         1.265274644, 1.230626273, 1.199838453, 1.16535383, 1.168612192,           # -- Re-indexed from 2012 to 2019
                         1.148046414, 1.119710706, 1.09922, 1.084535391, 1.068375985,
                         1.066003336, 1.055440335, 1.036501306, 1.01480825, 1,
                         0.988285008, 0.951458496 ))
  colnames(lookup) <-c("DATA_YEAR","PCE_i")
  dt <- setDT(df)
  setkey(dt, "DATA_YEAR")
  dt %<>% .[lookup, grep("sum|median|mean", colnames(.)):=lapply(.SD, function(x) round(x * PCE_i)), # Applies the appropriate multiplier
            .SDcols=grep("sum|median|mean", colnames(.)), on=key(.)] %>%                           # --to any sum/median/mean & their MOE
    setDF()
  return(dt)
}

# add_moe_length <- function(rs){    # This kind of helper should be in the visualization side
#   rs %<>% mutate(                  #   -- there's no additional data
#             share = round(share, 3),
#             share_moe = round(share_moe,3),
#             moe_length = 2 * share_moe)
# }

# 4. Main functions -------------------------------------------------

# Subject variables are defined within the primary function
#  -- but this could be separated into additional setup functions, if preferred

# Generate all indicators for a single survey
pums_efa_singleyear <- function(dyear, span=1){

  pp_df <- get_psrc_pums(span, dyear, "p", pvars)                                                  # Retrieve persons data
  pp_df %<>% add_efa_vars() %>% mutate(
               edu_simp = factor(case_when(AGEP<25                       ~ NA_character_,          # Define the educational attainment subject variable
                                    grepl("(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",
                                    !is.na(SCHL)                         ~ "Less than a Bachelor's degree")),
               healthcov = factor(case_when(AGEP<25                      ~ NA_character_,          # Define the health insurance coverage subject variable
                                    grepl("^With ", PRIVCOV)|grepl("^With ", PUBCOV) ~ "With health insurance",
                                    grepl("^Without ", PRIVCOV) & grepl("^Without ", PUBCOV) ~ "Without health insurance")))

  if(dyear<2020){hvars %<>% replace(hvars=="ACCESSINET","ACCESS")}                                 # Variable changed names w/ 2020 data
  hh_df <- get_psrc_pums(span, dyear, "h", hvars)                                                  # Retrieve household data
  if("ACCESS" %in% colnames(hh_df)){hh_df %<>% rename("ACCESSINET"="ACCESS")}                      # Variable changed names w/ 2020 data
  hh_df %<>% add_efa_vars() %>% mutate(
               poverty=Income_cat,                                                                 # Identical to Income_cat
               housing_burden=factor(case_when(                                                    # Define the rent burden subject variable
                                     GRPIP<30|OCPIP<30|SMOCP==0|(is.na(GRNTP) & is.na(SMOCP)) ~ "Less than 30 percent",
                                     between(GRPIP,30,50)|between(OCPIP,30,50) ~ "Between 30 and 50 percent",
                                     GRPIP>50|OCPIP>50|is.na(HINCP)      ~ "Greater than 50 percent"),
                                levels=c("Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent")),
               crowding = factor(case_when(is.na(NP)|is.na(BDSP)|BDSP==0 ~ NA_character_,          # Define the crowding subject variable
                                           NP/BDSP <= 1                  ~ "One person per bedroom or less",
                                           NP/BDSP <= 1.5                ~ "Between 1 and 1.5 person(s) per bedroom",
                                           NP/BDSP  > 1.5                ~ "More than 1.5 persons per bedroom"),
                                 levels=c("More than 1.5 persons per bedroom",
                                          "Between 1 and 1.5 person(s) per bedroom",
                                          "One person per bedroom or less")),
               internet = factor(case_when(grepl("^Yes", ACCESSINET)     ~ "With internet access", # Define the internet access subject variable; began in 2013
                                         grepl("^No", ACCESSINET)        ~ "Without internet access")))

  deep_pocket      <- vector(mode='list', length=9)                                                # Give list a length so empty elements can be named
  names(deep_pocket) <- c("educational_attainment",                                                # Name the elements; match the bulk_stat functions below
                          "healthcare_coverage",
                          "median_household_income",
                          "household_poverty",
                          "housing_cost_burden",
                          "median_gross_rent",
                          "crowding",
                          "SNAP",
                          "internet_access")
  deep_pocket[[1]] <- bulk_count_efa(pp_df, "edu_simp")                                            # Generate educational attainment table
  deep_pocket[[2]] <- bulk_count_efa(pp_df, "healthcov")                                           # Generate health insurance coverage table
  deep_pocket[[3]] <- bulk_stat_efa(hh_df, "median", "HINCP") %>% deflate_2019()                   # Generate median household income table; dollar comparisons require inflation adjustment
  deep_pocket[[4]] <- bulk_count_efa(hh_df, "poverty")                                             # Generate median household income table
  deep_pocket[[5]] <- bulk_count_efa(hh_df, "housing_burden")                                      # Generate housing cost burden table
  deep_pocket[[6]] <- bulk_stat_efa(hh_df, "median", "GRNTP") %>% deflate_2019()                   # Generate gross rent table
  deep_pocket[[7]] <- bulk_count_efa(hh_df, "crowding")                                            # Generate crowding (persons per bedroom) table
  deep_pocket[[8]] <- bulk_count_efa(hh_df, "FS")                                                  # Generate food stamp/SNAP table
  deep_pocket[[9]] <- bulk_count_efa(hh_df, "internet")                                            # Generate internet access table
  return(deep_pocket)
}

# Generate trend data, i.e. all indicators across multiple years
pums_efa_multiyear <- function(dyears){
  rs_master <- lapply(dyears, pums_efa_singleyear) %>%
    as.data.frame(do.call(rbind, lapply(., as.vector))) %>% setDT() %>% lapply(rbindlist)          # Combine matching indicator tables across years
  return(rs_master)                                                                                # Also return the object
}

# Write all tables to file
write_pums_efa <- function(efa_rs_list){
  mapply(fwrite, efa_rs_list, paste(names(efa_rs_list), ".csv", sep=""))
  return(NULL)
}

# Example 1: Generate indicators for a single year ------------------
# equity_2019_5 <- get_pums_efa(2019, 5)                                                           # Returns all tables as separate items in a list
# write_pums_efa(equity_2019_5)                                                                    # Write the tables to .csv

# Example 2: Generate annual indicators for 5 years -----------------
# equity_trend_2015_19 <- write_pums_efa_multiyear(2015:2019)                                      # Returns all tables as separate items in a list
# write_pums_efa(equity_trend_2015_19)                                                             # Write the tables to .csv                                                                                                   # --as well as returning all tables (as separate items in a list)

