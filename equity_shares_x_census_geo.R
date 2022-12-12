
# Functions ----------------------------------------------

using <- function(...) {                                                                           # Attaches libraries; installs if needed
  libs <- unlist(list(...))
  req  <- unlist(lapply(libs,require,character.only=TRUE))
  need <- libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

elmer_connect <-function(){dbConnect(odbc::odbc(),
                                     driver = "ODBC Driver 17 for SQL Server",
                                     server = "AWS-PROD-SQL\\Sockeye",
                                     database = "Elmer",
                                     trusted_connection = "yes")}

run_query <- function(conn, send_sql){
  rs <- dbSendQuery(conn, SQL(send_sql))
  dbClearResult(rs)
}

vars <- c("B02001_001","B02001_002",                                                               # ACS variables
          "C17002_001","C17002_008",
          "B22010_001","B22010_004","B22010_007",
          "B11005_001","B11005_002",
          "B11007_001","B11007_002",
          "C16002_001","C16002_004","C16002_007","C16002_010","C16002_013")

label_quintile <- function(var){
  breakpoints <- unique(quantile(var, probs=0:5/5, na.rm=TRUE))                                    # Unique so all-zero quintiles act as one bin
  rv <- cut(var, breaks=unique(breakpoints), labels=1:(length(breakpoints)-1),
    include.lowest=TRUE, right=TRUE)
  return(rv)
}

get_psrc_equity_shares <- function(dyear, scale){
  equity_dims   <- c("poc","income","disability","youth","older","lep")
  share_vars <- paste0(equity_dims,"_share")
  quintile_vars <- paste0(equity_dims,"_quintile")

  shares <- get_acs(geography=scale, variables=vars, state=53,
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
  sockeye_connection <- elmer_connect()
  table_id <- Id(schema = "stg", table = tblname)
  dbWriteTable(sockeye_connection, table_id, rs, overwrite = TRUE)                                 # Write staging table
  merge_sql <- paste("MERGE INTO equity.", tblname, " WITH (HOLDLOCK) AS target",                  # Insert new data
                     "USING stg.", tblname, " AS source",
                     "ON target.data_year=source.data_year AND target.geoid=source.geoid",
                     "WHEN NOT MATCHED BY TARGET THEN INSERT (", paste0(colnames(rs), collapse=", "),")",
                     "VALUES (", paste0("source.", colnames(rs), collapse=", "), ");")
  run_query(sockeye_connection, merge_sql)                                                         # Execute the query
  run_query(sockeye_connection, paste0("DROP TABLE stg.", tblname))                                # Clean up
  dbDisconnect(sockeye_connection)
}

# Examples -------------------------------------------------------
using("magrittr","data.table", "tidycensus","odbc","DBI")                                          # Libraries

data_year <- 2020                                                                                  # Ending year of desired 5-yr ACS data

# rs <- get_psrc_equity_shares(data_year,"block group")
# filename <- paste0(getwd(),"/equity_shares.csv")
# fwrite(equity_shares, filename)                                                                  # Write to file

write_shares_to_elmer(data_year,"block group")                                                     # Write to Elmer
