library(magrittr)
library(tidycensus)
library(psrcelmer)
library(data.table)

# Functions ----------------------------------------------

# ACS variables
choose_acs_varids <- function(dyear){
  var_ids <- c("B03002_001","B03002_003",                                                          # POC / race
               "C17002_001","C17002_008",                                                          # income
               "B11005_001","B11005_002",                                                          # presence of youth in household
               "B11007_001","B11007_002")                                                          # presence of older in household
  if(dyear>2011){
    var_ids %<>% c(c("B22010_001","B22010_004","B22010_007"))                                      # disability - 2012+
  }
  if(dyear>2015){
    var_ids %<>% c(c("C16002_001","C16002_004","C16002_007","C16002_010","C16002_013"))            # limited English proficiency - 2016+
  }
return(var_ids)
}

choose_dims_n_vars <- function(dyear){
  equity_dims   <- c("poc","income",
                     "youth","older")
  if(dyear>2011){ equity_dims %<>% c("disability") }
  if(dyear>2015){ equity_dims %<>% c("lep") }
  df <- data.frame(dims=equity_dims,
                   share_vars <- paste0(equity_dims,"_share"),
                   quintile_vars <- paste0(equity_dims,"_quintile"))
return(df)
}

cty_codes <- data.table( fips=c("033","035","053","061"),
                         county=c("King","Kitsap","Pierce","Snohomish"))

label_quintile <- function(var) {
  breakpoints <- unique(quantile(var, probs = 0:5/5, na.rm = TRUE))
  num_intervals <- length(breakpoints) - 1
  if (num_intervals <= 0) {
    # no variation or all NA
    return(rep(NA_real_, length(var)))
  }
  labels <- (6 - num_intervals):5
  
  cut_fac <- cut(var, breaks = breakpoints, labels = labels,
                 include.lowest = TRUE, right = TRUE)
  # Convert factor of labels to their numeric label values
  q_int <- as.numeric(as.character(cut_fac))
  
  if (num_intervals < 5) {
    lowest_label <- min(labels)             # e.g., 2 when k=4; 3 when k=3
    q_num <- ifelse(q_int == lowest_label,  # only lowest reported bin
                    q_int - 0.5,            # becomes 1.5 or 2.5, etc.
                    q_int)                  # all others unchanged
  } else {
    q_num <- q_int
  }
  return(q_num)
}

get_psrc_equity_shares <- function(dyear, entirety){                                               # dyear - last year of 5yr ACS;
                                                                                                   # entirety - either "region" or "county"
  dimvar_df <- choose_dims_n_vars(dyear)

  shares <- get_acs(geography="tract", variables=choose_acs_varids(dyear), state=53,               # Retrieves the data
                    county=cty_codes$fips, year=dyear, survey="acs5") %>%
    setDT %>% dcast(GEOID ~ variable, value.var="estimate") %>%
    .[B03002_001 > 0,`:=`(Year=..dyear, county=stringr::str_sub(GEOID,3,5),
                          poc_share= (B03002_001 - B03002_003) / B03002_001,                       # Calculate share per block group
                          income_share= (C17002_001 - C17002_008) / C17002_001,
                          youth_share= B11005_002 / B11005_001,
                          older_share= B11007_002 / B11007_001
                          )]
  if(dyear>2011){
    shares[B03002_001 > 0,`:=`(disability_share= (B22010_001-B22010_004-B22010_007) / B22010_001)]
    }
  if(dyear>2015){
    shares[B03002_001 > 0,`:=`(lep_share= (C16002_004 + C16002_007 + C16002_010 + C16002_013) / C16002_001)]
  }
  shares %<>% .[, which(grepl("\\d$", colnames(.))):=NULL] %>%
       .[, lapply(.SD, function(x) replace(x, is.nan(x), NA))] %>% .[!is.na(Year)]                 # Replace NaN
  if(entirety=="county"){                                                                          # Add county-level quintile value
    shares[county=='033', (dimvar_df$quintile_vars):=lapply(.SD, label_quintile), .SDcols=dimvar_df$share_vars]
    shares[county=='035', (dimvar_df$quintile_vars):=lapply(.SD, label_quintile), .SDcols=dimvar_df$share_vars]
    shares[county=='053', (dimvar_df$quintile_vars):=lapply(.SD, label_quintile), .SDcols=dimvar_df$share_vars]
    shares[county=='061', (dimvar_df$quintile_vars):=lapply(.SD, label_quintile), .SDcols=dimvar_df$share_vars]
    shares %<>% .[cty_codes, county:=county, on =.(county=fips)]
  }else if(entirety=="region"){
    shares[, (dimvar_df$quintile_vars):=lapply(.SD, label_quintile), .SDcols=dimvar_df$share_vars] # Add regional quintile value
    shares[, county:="Region"]
  }
  shares %<>% setnames(old=c("GEOID","Year"), new=c("geoid", "data_year")) %>%
  return(shares)
}

write_shares_to_elmer <- function(df){

  dyear <- unique(df$data_year)[1]                                                                 # Extract year from the data
  dimvar_df <- choose_dims_n_vars(dyear)                                                           # -- use it to determine dimensions
  df_colnames <- colnames(df)
  
  # Create the merge SQL syntax
  merge_sql <- paste("MERGE INTO equity.tract_shares WITH (HOLDLOCK) AS target",
                     "USING stg.tract_shares AS source",
                     "ON target.data_year = source.data_year AND target.geoid = source.geoid AND target.county = source.county",
                     "WHEN MATCHED THEN UPDATE SET",
                     paste0(paste0(dimvar_df$share_vars,"=source.", dimvar_df$share_vars, collapse=", "), ", ",
                            paste0(dimvar_df$quintile_vars,"=source.", dimvar_df$quintile_vars, collapse=", ")),
                     "WHEN NOT MATCHED BY TARGET THEN INSERT (", paste0(df_colnames, collapse=", "),")",
                     "VALUES (", paste0("source.", df_colnames, collapse=", "), ");")
  
  psrcelmer::stage_table(df, "tract_shares")                                                       # Stage table first                                                   
  psrcelmer::sql_execute(sql=merge_sql)                                                            # -- then merge                                                   
  psrcelmer::sql_execute(sql=paste("DROP TABLE stg.tract_shares"))                                 # Clean up                              
  return(invisible(NULL))                                                                          # No return object for this action                             
}

# Examples -------------------------------------------------------

# Write to file
# rs <- get_psrc_equity_shares(2020,"region")
# filename <- paste0(getwd(),"/equity_shares.csv")
# fwrite(equity_shares, filename)

# Write to Elmer
args <- expand.grid(years= c(2023), entirety=c("county","region"))
tract_shares <- list()
tract_shares <- mapply(get_psrc_equity_shares, args$years, args$entirety, SIMPLIFY = FALSE)
lapply(tract_shares, write_shares_to_elmer)
