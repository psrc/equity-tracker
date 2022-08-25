dyear <- 2020                                                                                      # Editable; ending year of desired 5-yr ACS data

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

# --------------------------------------------------------

using("magrittr","data.table", "tidycensus")                                                       # Libraries

equity_shares <- get_acs(geography="block group", variables=vars, state=53,
                         county=c("033","035","053","061"), year=dyear, survey="acs5") %>%
  setDT %>% dcast(GEOID ~ variable, value.var="estimate") %>%
   .[B02001_001 > 0,`:=`(POC_share= (B02001_001 - B02001_002) / B02001_001,                        # Calculate share per block group
                         Income_share= (C17002_001 - C17002_008) / C17002_001,
                         Disability_share= (B22010_001-B22010_004-B22010_007) / B22010_001,
                         Youth_share= B11005_002 / B11005_001,
                         Older_share= B11007_002 / B11007_001,
                         LEP_share= (C16002_004 + C16002_007 + C16002_010 + C16002_013) / C16002_001)] %>%
   .[, which(grepl("\\d$", colnames(.))):=NULL] %>% .[complete.cases(.)]                           # Remove original columns & NA rows
equity_dims   <- c("POC","Income","Disability","Youth","Older","LEP")
share_vars <- paste0(equity_dims,"_share")
quintile_vars <- paste0(equity_dims,"_quintile")
equity_shares[, (quintile_vars):=lapply(.SD, label_quintile), .SDcols=share_vars]                  # Add quintile label

filename <- paste0(getwd(),"/equity_shares.csv")
fwrite(equity_shares, filename)                                                                    # Write to file
