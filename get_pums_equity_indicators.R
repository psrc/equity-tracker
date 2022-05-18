library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(data.table)

# Setup: variables --------------------------------------------------

pvars <- c("AGEP",                   # used as condition >25yo
           "DIS",                    # Disability
           "PRACE",                  # PSRC individual race category
           "POVPIP",                 # Income-to-poverty ratio
           "R18",                    # Presence of persons under 18 years in household
           "R65",                    # Presence of persons over 65 years in household
           "SCHL",                   # Educational attainment
           "LNGI")                   # Limited English speaking household

efa_vars <- c("POC_cat", "Income_cat", "Disability_cat", "Youth_cat", "Older_cat", "LEP_cat")

recode_equity_vars <- function(so){
  so_plus <- mutate(so,
    edu_simp    = factor(case_when(AGEP<25                       ~ NA_character_,
                            grepl("(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",
                            !is.na(SCHL)                         ~ "Less than a Bachelor's degree")),
    POC_cat     = factor(case_when(PRACE=="White alone"          ~ "Non-POC",
                            !is.na(PRACE)                        ~ "POC"),
                         levels=c("POC","Non-POC")),
    Income_cat  = factor(case_when(POVPIP<200                    ~ "Low Income",
                            !is.na(POVPIP)                       ~ "Non-Low Income")),
    Disability_cat=factor(case_when(DIS=="With a disability"     ~ "Disability",
                            !is.na(DIS)                          ~ "Non-disability")),
    Youth_cat   = factor(case_when(grepl("^1 or more", R18)      ~ "Youth",
                            !is.na(R18)                          ~ "Non-youth"),
                         levels=c("Youth","Non-youth")),
    Older_cat   = factor(case_when(grepl("^1 or|^2 or", R65)     ~ "Older adult",
                            !is.na(R65)                          ~ "Non-older adult"),
                         levels=c("Older adult","Non-older adult")),
    LEP_cat     = factor(case_when(grepl("^No one", LNGI)        ~ "Limited English proficiency",
                            !is.na(LNGI)                         ~ "English proficient")))
  return(so_plus)
}

# Setup: Functions --------------------------------------------------

add_moe_length <- function(rs){
  rs %<>% mutate(
            share = round(share, 3),
            share_moe = round(share_moe,3),
            moe_length = 2 * share_moe)
}

# Main function start -----------------------------------------------

get_pums_equity_indicators <- function(span, dyear){

  pp_df <- get_psrc_pums(span, dyear, "p", pvars)                                                  # Retrieve data
  pp_df %<>% recode_equity_vars()                                                                  # Add custom variables
  deep_pocket <- list()                                                                            # Container to hold results

# Calculate EFA educational attainment ------------------------------

  efa_vars_rgn <- efa_vars %>% lapply(FUN=function(x) c(x, "edu_simp"))                            # Regional variable set
  efa_vars_cty <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY","edu_simp"))                   # County variable set

  efa_results <- list()
  efa_results[[1]] <- pums_bulk_stat(pp_df, "count", group_var_list=efa_vars_rgn, incl_na=FALSE)
  efa_results[[2]] <- pums_bulk_stat(pp_df, "count", group_var_list=efa_vars_cty, incl_na=FALSE) %>%
                         .[COUNTY!="Region"]
  deep_pocket[[1]] <- rbindlist(efa_results) %>% add_moe_length() %>% arrange(DATA_YEAR, var_name, COUNTY)


  # # setting up median stat function
  # # includes inflation to 2019 values (https://www.bls.gov/data/inflation_calculator.htm)
  # race_eth_pums <- function(dt, year){
  #   race <- psrc_pums_median(dt, "HINCP", c("PRACE", "COUNTY")) %>%
  #     rename(RaceHisp=PRACE) %>%
  #     rename(Year=DATA_YEAR) %>%
  #     mutate(median_no_infl=HINCP_median) %>%
  #     mutate(moe_no_infl=HINCP_median_moe)%>%
  #     mutate(infl_val=case_when(Year==2019 ~1,
  #                               Year==2018 ~ 1.0155,
  #                               Year==2017 ~ 1.0365,
  #                               Year==2016 ~ 1.0625,
  #                               Year==2015 ~ 1.0770)) %>%
  #     mutate(HINCP_median=median_no_infl*infl_val) %>%
  #     mutate(HINCP_median_moe=moe_no_infl*infl_val) %>%
  #     mutate(HINCP_median_round=round(HINCP_median,-2)) %>%
  #     mutate(HINCP_median_moe_round=round(HINCP_median_moe,-2)) %>%
  #     filter(!is.na(HINCP_median_moe)) %>%
  #     return()
  # }







}
