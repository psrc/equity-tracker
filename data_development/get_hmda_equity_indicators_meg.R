library(magrittr)
library(data.table)
library(httr)
library(jsonlite)
library(psrcplot)

# Helper attributes / functions -------------------------------------

races_vector <- c("Asian",                                                                         # Races listed on HMDA
                  "Native Hawaiian or Other Pacific Islander",
                  "Free Form Text Only",
                  "Race Not Available",
                  "American Indian or Alaska Native",
                  "Black or African American",
                  "2 or more minority races",
                  "White",
                  "Joint")                                                                        # Recode spaces, etc.

poc_vector <- paste0(races_vector[c(1:3,5:6,9)], collapse=",") %>% c("White") %>%
  lapply(URLencode)

ethnicities_vector <- c("Hispanic or Latino",
                       "Not Hispanic or Latino",
                       "Joint",
                       "Ethnicity Not Available",
                       "Free Form Text Only") 

## Meg start -----------------------------------------

# Pull csv in from site and download
## MAKE SURE TO ADD COLUMN TO THE LEFT HAND SIDE AND ADD MEDIAN INCOME TO LEFT COLUMN
kitsap_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD14740_BremertonSilverdalePortOrchard.csv")
king_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD42644_SeattleBellevueKent.csv")
pierce_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD45104_TacomaLakewood.csv")

# Clean data file - kitsap
# remove extra columns
kitsap_clean <- kitsap_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
kitsap_clean$denied <- as.numeric(as.character(kitsap_clean$denied))
kitsap_clean$received <- as.numeric(as.character(kitsap_clean$received))

#class(kitsap_clean$received)

# reorganize data and include data year
kitsap_clean$share_denials <- kitsap_clean$denied/kitsap_clean$received
kitsap_clean$data_year <- 2021
kitsap_clean$region <- "kitsap"
kitsap_clean <- kitsap_clean[c(6, 7, 1, 2, 3, 4, 5)]

# pivot data set
#test_pivot <- kitsap_clean %>%
 # pivot_longer(cols = c("race_ethnicity"), 
  #             names_to = "equity_groups",
   #            values_to = "identifiers")

# Clean data file - king
# remove extra columns
king_clean <- king_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
king_clean$denied <- as.numeric(as.character(king_clean$denied))
king_clean$received <- as.numeric(as.character(king_clean$received))

#class(king_clean$received)

# reorganize data and include data year
king_clean$share_denials <- king_clean$denied/king_clean$received
king_clean$data_year <- 2021
king_clean$region <- "king"
king_clean <- king_clean[c(6, 7, 1, 2, 3, 4, 5)]

# Clean data file - pierce
# remove extra columns
pierce_clean <- pierce_raw[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
pierce_clean$denied <- as.numeric(as.character(pierce_clean$denied))
pierce_clean$received <- as.numeric(as.character(pierce_clean$received))

#class(pierce_clean$received)

# reorganize data and include data year
pierce_clean$share_denials <- pierce_clean$denied/pierce_clean$received
pierce_clean$data_year <- 2021
pierce_clean$region <- "pierce"
pierce_clean <- pierce_clean[c(6, 7, 1, 2, 3, 4, 5)]

# join datasets across region -----------------------------------------

region_clean <- rbind(kitsap_clean, king_clean, pierce_clean)

# plot data -----------------------------------------

facet_chart <- static_facet_line_chart(t=region_clean,
                        x="share_denials", y="race_ethnicity",
                        fill="race_ethnicity", facet="median_income",
                        color="psrc_light",
                        est ="percent",
                        scales="fixed")
facet_chart

# Export data

#work_book <- createWorkbook()
#addWorksheet(work_book, sheetName = "median sale price")
#writeData(work_book, "median sale price", county_piv)
#saveWorkbook(work_book, file = "Affordability Measures/r_output median_price_county.xlsx", overwrite = TRUE)
