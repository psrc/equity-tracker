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
hvars <- c("ACCESS",                 # Internet access
           "HDIS",                   # Any member of household disabled
           "HINCP",                  # Household income
           "HRACE",                  # Household race (PSRC categories)
           "GRPIP",                  # Gross rent as percentage of income
           "LNGI",                   # Limited English speaking household
           "POVPIP",                 # Income-to-poverty ratio
           "PRACE",                  # Householder race (PSRC categories)
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
  dt %<>% .[lookup, grep("sum|median|mean", colnames(.)):=lapply(.SD, function(x) x * PCE_i),      # Applies the appropriate multiplier
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
get_pums_efa <- function(dyear, span=1){

  pp_df <- get_psrc_pums(span, dyear, "p", pvars)                                                  # Retrieve persons data
  pp_df %<>% add_efa_vars() %>% mutate(
               edu_simp = factor(case_when(AGEP<25                       ~ NA_character_,          # Define the educational attainment subject variable
                                    grepl("(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",
                                    !is.na(SCHL)                         ~ "Less than a Bachelor's degree")),
               healthcov = factor(case_when(AGEP<25                      ~ NA_character_,          # Define the health insurance coverage subject variable
                                    grepl("^With ", PRIVCOV)|grepl("^With ", PUBCOV) ~ "With health insurance",
                                    grepl("^Without ", PRIVCOV) & grepl("^Without ", PUBCOV) ~ "Without health insurance")))

  hh_df <- get_psrc_pums(span, dyear, "h", hvars)                                                  # Retrieve household data
  hh_df %<>% add_efa_vars() %>% mutate(
             rent_burden=factor(case_when((TEN=="Rented" & HINCP<=0)     ~ "No income",            # Define the rent burden subject variable
                                          GRPIP < 30                     ~ "Less than 30 percent",
                                          between(GRPIP,30,50)           ~ "Between 30 and 50 percent",
                                          GRPIP > 50                     ~ "Greater than 50 percent"),
                                levels=c("No income",                                              # -- the `levels` argument allows ordering of results
                                         "Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent")),
             internet = factor(case_when(grepl("^Yes",ACCESS)            ~ "With internet access", # Define the internet access subject variable
                                         grepl("^No", ACCESS)            ~ "Without internet access")))

  deep_pocket      <- list()
  deep_pocket[[1]] <- bulk_count_efa(pp_df, "edu_simp")                                            # Generate the 1st indicator table
  deep_pocket[[2]] <- bulk_count_efa(pp_df, "healthcov")                                           # Generate the 2nd indicator table
  deep_pocket[[3]] <- bulk_stat_efa(hh_df, "median", "HINCP") %>% deflate_2019()                   # Generate the 3rd indicator table
  deep_pocket[[4]] <- bulk_count_efa(hh_df, "rent_burden")                                         # Generate the 4th indicator table
  deep_pocket[[5]] <- bulk_count_efa(hh_df, "internet")                                            # Generate the 5th indicator table
  ## Add additional indicators here as warranted

  return(deep_pocket)
}

# Generate trend data, i.e. all indicators across multiple years
write_pums_efa_multiyear <- function(dyears){
  rs_master <- lapply(dyears, get_pums_efa) %>%
    as.data.frame(do.call(rbind, lapply(., as.vector))) %>% setDT() %>% lapply(rbindlist)          # Combine matching indicator tables across years
  fwrite(rs_master[[1]], "edu_simp.csv")                                                           # Write each .csv files to working directory
  fwrite(rs_master[[2]], "healthcov.csv")
  fwrite(rs_master[[3]], "HINCP.csv")
  fwrite(rs_master[[4]], "rent_burden.csv")
  fwrite(rs_master[[5]], "internet.csv")
  return(rs_master)
}

# Example 1: Generate indicators for a single year ------------------
# equity_2019_5 <- get_pums_efa(2019, 5)                             # Returns all tables as separate items in a list
# fwrite(equity_2019_5[[1]], "edu_simp.csv")                         # Separate command to save the first subject table as .csv file

# Example 2 Generate annual indicators for 10 years -----------------
# equity_trend_2010_19 <- write_pums_efa_multiyear(2010:2019)        # Automatically writes all indicators to .csv
                                                                     # --as well as returning all tables (as separate items in a list)

