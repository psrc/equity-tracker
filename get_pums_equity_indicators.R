library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(data.table)

# Setup: variables --------------------------------------------------

pvars <- c("AGEP",                   # used as condition >25yo
           "DIS",                    # Disability
           "LNGI",                   # Limited English speaking household
           "PRACE",                  # Individual race (PSRC categories)
           "POVPIP",                 # Income-to-poverty ratio
           "PRIVCOV",                # Private health insurance coverage
           "PUBCOV",                 # Public health insurance coverage
#           "R18",                    # Presence of persons under 18 years in household
#           "R65",                    # Presence of persons over 65 years in household
           "SCHL")                   # Educational attainment

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


efa_vars <- c("POC_cat", "Income_cat", "Disability_cat", "LEP_cat") # "Youth_cat", "Older_cat",

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

# Setup: Helper functions -------------------------------------------

bulk_count_efa <- function(so, anal_var){
  reglist <- efa_vars %>% lapply(FUN=function(x) c(x, anal_var))
  ctylist <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY", anal_var))
  rs      <- list()
  rs[[1]] <- pums_bulk_stat(so, "count", group_var_list=reglist, incl_na=FALSE)
  rs[[2]] <- pums_bulk_stat(so, "count", group_var_list=ctylist, incl_na=FALSE) %>%
                .[COUNTY!="Region"]
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, var_name, COUNTY)
  return(rs)
}

bulk_stat_efa <- function(so, stat_type, anal_var){
  ctylist  <- efa_vars %>% lapply(FUN=function(x) c(x, "COUNTY"))
  rs       <- list()
  rs[[1]]  <- pums_bulk_stat(so, stat_type, anal_var, efa_vars, incl_na=FALSE)
  rs[[2]]  <- pums_bulk_stat(so, stat_type, anal_var, ctylist, incl_na=FALSE) %>%
                 .[COUNTY!="Region"]
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, var_name, COUNTY)
  return(rs)
}

deflate_2019 <- function(df){
  lookup <- data.table(c(2000:2021),
                       c(1.407632219, 1.379958823, 1.362072811, 1.334037234, 1.301730161,
                         1.265274644, 1.230626273, 1.199838453, 1.16535383, 1.168612192,
                         1.148046414, 1.119710706, 1.09922, 1.084535391, 1.068375985,
                         1.066003336, 1.055440335, 1.036501306, 1.01480825, 1,
                         0.988285008, 0.951458496 ))
  colnames(lookup) <-c("DATA_YEAR","PCE_i")
  dt <- setDT(df)
  setkey(dt, "DATA_YEAR")
  dt %<>% .[lookup, grep("sum|median|mean", colnames(.)):=lapply(.SD, function(x) x * PCE_i),
            .SDcols=grep("sum|median|mean", colnames(.)), on=key(.)] %>%
    setDF()
  return(dt)
}

# add_moe_length <- function(rs){    # This kind of helper should be in the visualization side
#   rs %<>% mutate(                  #   -- there's no additional data
#             share = round(share, 3),
#             share_moe = round(share_moe,3),
#             moe_length = 2 * share_moe)
# }

# Overarching function start ----------------------------------------

get_pums_efa <- function(dyear, span=1){

  pp_df <- get_psrc_pums(span, dyear, "p", pvars)                                                  # Retrieve persons data
  pp_df %<>% add_efa_vars() %>% mutate(
               edu_simp = factor(case_when(AGEP<25                       ~ NA_character_,
                                    grepl("(Bach|Mast|Prof|Doct)", SCHL) ~ "Bachelor's degree or higher",
                                    !is.na(SCHL)                         ~ "Less than a Bachelor's degree")),
               healthcov = factor(case_when(AGEP<25                      ~ NA_character_,
                                    grepl("^With ", PRIVCOV)|grepl("^With ", PUBCOV) ~ "With health insurance",
                                    grepl("^Without ", PRIVCOV)|grepl("^Without ", PUBCOV) ~ "Without health insurance")))

  hh_df <- get_psrc_pums(span, dyear, "h", hvars)                                                  # Retrieve household data
  hh_df %<>% add_efa_vars() %>% mutate(
             rent_burden=factor(case_when((TEN=="Rented" & HINCP<=0)     ~"No income",
                                          GRPIP < 30                     ~"Less than 30 percent",
                                          between(GRPIP,30,50)           ~ "Between 30 and 50 percent",
                                          GRPIP > 50                     ~ "Greater than 50 percent"),
                                levels=c("No income",
                                         "Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent")),
             internet = factor(case_when(grepl("^Yes",ACCESS)            ~ "With internet access",
                                         grepl("^No", ACCESS)            ~ "Without internet access")))

  deep_pocket      <- list()
  deep_pocket[[1]] <- bulk_count_efa(pp_df, "edu_simp")
  deep_pocket[[2]] <- bulk_count_efa(pp_df, "healthcov")
  deep_pocket[[3]] <- bulk_stat_efa(hh_df, "median", "HINCP") %>% deflate_2019()
  deep_pocket[[4]] <- bulk_count_efa(hh_df, "rent_burden")
  deep_pocket[[5]] <- bulk_count_efa(hh_df, "internet")
  ## Add additional indicators here as warranted

  return(deep_pocket)
}

write_pums_efa_multiyear <- function(dyears){
  rs_master <- lapply(dyears, get_pums_efa) %>%
    as.data.frame(do.call(rbind, lapply(., as.vector))) %>% setDT() %>% lapply(rbindlist)
  fwrite(rs_master[[1]], "edu_simp.csv")
  fwrite(rs_master[[2]], "healthcov.csv")
  fwrite(rs_master[[3]], "HINCP.csv")
  fwrite(rs_master[[4]], "rent_burden.csv")
  fwrite(rs_master[[5]], "internet.csv")
  return(rs_master)
}

