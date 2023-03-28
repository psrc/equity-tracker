library(magrittr)
library(tidycensus)
library(psrcelmer)
library(data.table)

# Functions ----------------------------------------------

# ACS variables
vars <- c("B02001_001","B02001_002",                                                               # POC / race
          "C17002_001","C17002_008",                                                               # income
          "B22010_001","B22010_004","B22010_007",                                                  # disability
          "B11005_001","B11005_002",                                                               # presence of youth in household
          "B11007_001","B11007_002",                                                               # presence of older in household
          "C16002_001","C16002_004","C16002_007","C16002_010","C16002_013")                        # limited English proficiency

label_quintile <- function(var){
  breakpoints <- unique(quantile(var, probs=0:5/5, na.rm=TRUE))                                    # Unique so all-zero quintiles act as one bin
  rv <- cut(var, breaks=unique(breakpoints), labels=1:(length(breakpoints)-1),
    include.lowest=TRUE, right=TRUE)
  return(rv)
}

get_psrc_equity_shares <- function(dyear, scale){                                                   # dyear - last year of 5yr ACS;
  equity_dims   <- c("poc","income","disability","youth","older","lep")                             # scale - either "block group" or "tract"
  share_vars <- paste0(equity_dims,"_share")
  quintile_vars <- paste0(equity_dims,"_quintile")

  shares <- get_acs(geography=scale, variables=vars, state=53,                                      # Retrieves the data
                    county=c("033","035","053","061"), year=dyear, survey="acs5") %>%
    setDT %>% dcast(GEOID ~ variable, value.var="estimate") %>%
    .[B02001_001 > 0,`:=`(Year=..dyear, poc_share= (B02001_001 - B02001_002) / B02001_001,          # Calculate share per block group
                          income_share= (C17002_001 - C17002_008) / C17002_001,
                          disability_share= (B22010_001-B22010_004-B22010_007) / B22010_001,
                          youth_share= B11005_002 / B11005_001,
                          older_share= B11007_002 / B11007_001,
                          lep_share= (C16002_004 + C16002_007 + C16002_010 + C16002_013) / C16002_001)] %>%
    .[, which(grepl("\\d$", colnames(.))):=NULL] %>%
    .[, lapply(.SD, function(x) replace(x, is.nan(x), NA))] %>%                                    # Replace NaN
    .[, (quintile_vars):=lapply(.SD, label_quintile), .SDcols=share_vars] %>%                      # Add quintile value
    setnames(old=c("GEOID","Year"), new=c("geoid", "data_year"))
  return(shares)
}

write_shares_to_elmer <- function(dyear, scale){
  tblname <- scale %>% gsub(" ", "", .) %>% paste0("_shares")
  rs <- get_psrc_equity_shares(dyear, scale)

  merge_sql <- paste("MERGE INTO equity.", tblname, " WITH (HOLDLOCK) AS target",                  # Create the merge SQL syntax
                     "USING stg.", tblname, " AS source",
                     "ON target.data_year=source.data_year AND target.geoid=source.geoid",
                     "WHEN NOT MATCHED BY TARGET THEN INSERT (", paste0(colnames(rs), collapse=", "),")",
                     "VALUES (", paste0("source.", colnames(rs), collapse=", "), ");")

  psrcelmer::stage_table(tblname)                                                                  # Stage table first
  psrcelmer::sql_execute(sql=merge_sql)                                                            # -- then merge
  psrcelmer::sql_execute(sql=paste("DROP TABLE stg.", tblname))                                    # Clean up
  return(invisible(NULL))                                                                          # No return object for this action
}

# Examples -------------------------------------------------------
# function arguments:
#     dyear - last year of 5yr ACS;
#     scale - either "block group" or "tract"

# Write to file
# rs <- get_psrc_equity_shares(2020,"block group")
# filename <- paste0(getwd(),"/equity_shares.csv")
# fwrite(equity_shares, filename)

# Write to Elmer
write_shares_to_elmer(2020,"block group")
