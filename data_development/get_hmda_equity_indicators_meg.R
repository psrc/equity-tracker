library(magrittr)
library(data.table)
library(httr)
library(jsonlite)
library(dplyr)

# packages that are from github that host functions for pulling data (do the install in R Gui, not RStudio)

devtools::install_github("psrc/psrc.travelsurvey", force = TRUE)
devtools::install_github("psrc/psrccensus", force = TRUE)
devtools::install_github("psrc/psrcplot", force = TRUE)
devtools::install_github("psrc/psrctrends", force = TRUE)

# run these after installing github changes through R Gui

library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
library(psrctrends)

install_psrc_fonts()

# Helper attributes / functions from Michael -------------------------------------

races_vector <- c("Asian",                                                                         # Races listed on HMDA
                  "Black or African American",
                  "Hispanic or Latino",
                  "White")                                                                        # Re-code spaces, etc.

# run these after installing github changes through R Gui

poc_vector <- paste0(races_vector[c(1:3,5:6,9)], collapse=",") %>% c("White") %>%
  lapply(URLencode)

ethnicities_vector <- c("Hispanic or Latino",
                       "Not Hispanic or Latino",
                       "Joint",
                       "Ethnicity Not Available",
                       "Free Form Text Only") 

# Pull csv in from site and download ------- 2018
#BremSilverPOrchard_raw_18 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2018/MSAMD14740_BremertonSilverdalePortOrchard.csv")
#SeattleBellevue_raw_18 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2018/MSAMD42644_SeattleBellevueKent.csv")
#TacomaLake_raw_18 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2018/MSAMD45104_TacomaLakewood.csv")

# Pull csv in from site and download ------- 2019
#BremSilverPOrchard_raw_19 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2019/MSAMD14740_BremertonSilverdalePortOrchard.csv")
#SeattleBellevue_raw_19 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2019/MSAMD42644_SeattleBellevueKent.csv")
#TacomaLake_raw_19 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2019/MSAMD45104_TacomaLakewood.csv")

# Pull csv in from site and download ------- 2020
#BremSilverPOrchard_raw_20 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2020/MSAMD14740_BremertonSilverdalePortOrchard.csv")
#SeattleBellevue_raw_20 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2020/MSAMD42644_SeattleBellevueKent.csv")
#TacomaLake_raw_20 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2020/MSAMD45104_TacomaLakewood.csv")

# Pull csv in from site and download ------- 2021
## MAKE SURE TO ADD COLUMN TO THE LEFT HAND SIDE AND ADD MEDIAN INCOME TO LEFT COLUMN
BremSilverPOrchard_raw_21 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD14740_BremertonSilverdalePortOrchard.csv")
SeattleBellevue_raw_21 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD42644_SeattleBellevueKent.csv")
TacomaLake_raw_21 <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC/2021/MSAMD45104_TacomaLakewood.csv")

# Clean data file - BremSilverPOrchard
# remove extra columns
BremSilverPOrchard_clean <- BremSilverPOrchard_raw_21[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
BremSilverPOrchard_clean$denied <- as.numeric(as.character(BremSilverPOrchard_clean$denied))
BremSilverPOrchard_clean$received <- as.numeric(as.character(BremSilverPOrchard_clean$received))

# reorganize data and include data year
BremSilverPOrchard_clean$share_denials_race <- BremSilverPOrchard_clean$denied/BremSilverPOrchard_clean$received
BremSilverPOrchard_clean$data_year <- 2021
BremSilverPOrchard_clean$region <- "BremSilverPOrchard"
BremSilverPOrchard_clean <- BremSilverPOrchard_clean[c(6, 7, 1, 2, 3, 4, 5)]

# pivot data set
#test_pivot <- BremSilverPOrchard_clean %>%
 # pivot_longer(cols = c("race_ethnicity"), 
  #             names_to = "equity_groups",
   #            values_to = "identifiers")

# Clean data file - SeattleBellevue
# remove extra columns
SeattleBellevue_clean <- SeattleBellevue_raw_21[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
SeattleBellevue_clean$denied <- as.numeric(as.character(SeattleBellevue_clean$denied))
SeattleBellevue_clean$received <- as.numeric(as.character(SeattleBellevue_clean$received))

#class(SeattleBellevue_clean$received)

# reorganize data and include data year
SeattleBellevue_clean$share_denials_race <- SeattleBellevue_clean$denied/SeattleBellevue_clean$received
SeattleBellevue_clean$data_year <- 2021
SeattleBellevue_clean$region <- "SeattleBellevue"
SeattleBellevue_clean <- SeattleBellevue_clean[c(6, 7, 1, 2, 3, 4, 5)]

setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

write.csv(SeattleBellevue_clean, "Loan Denials- FFIEC/r_output 2021 1-year.csv", row.names=FALSE)

# Clean data file - TacomaLake
# remove extra columns
TacomaLake_clean <- TacomaLake_raw_21[-c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81), c(1, 2, 3, 9)] %>%
  rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
         "received" = "Applications.Received", 
         "denied" = "Applications.Denied",
         "median_income" = "X")

# change from character to numeric for calculation of share denials
TacomaLake_clean$denied <- as.numeric(as.character(TacomaLake_clean$denied))
TacomaLake_clean$received <- as.numeric(as.character(TacomaLake_clean$received))

#class(TacomaLake_clean$received)

# reorganize data and include data year
TacomaLake_clean$share_denials_race <- TacomaLake_clean$denied/TacomaLake_clean$received
TacomaLake_clean$data_year <- 2021
TacomaLake_clean$region <- "TacomaLake"
TacomaLake_clean <- TacomaLake_clean[c(6, 7, 1, 2, 3, 4, 5)]

# join datasets across region -----------------------------------------

region_clean <- rbind(BremSilverPOrchard_clean, SeattleBellevue_clean, TacomaLake_clean)

#create column for specific charts/plots ---------------------------------

# chart #1 ----------------------------------
region_race_21 <- region_clean[region_clean$race_ethnicity %in% c("American Indian or Alaska Native", "Asian",
                                                                  "Black or African American", "Hispanic or Latino",
                                                                  "Native Hawaiian or Other Pacific Islander",
                                                                  "2 or more minority races", "White"), ] %>%
  mutate(race_ethnicity= case_when(race_ethnicity == "Black or African American" ~ "Black",
                                   race_ethnicity == "2 or more minority races" ~ "Two or more non-white races",
                                   TRUE ~ as.character(race_ethnicity))) %>%
  mutate(race_ethnicity = fct_relevel(race_ethnicity, 
                                      "American Indian or Alaska Native", "Asian",
                                      "Black", "Hispanic or Latino",
                                      "Native Hawaiian or Other Pacific Islander",
                                      "Two or more non-white races", "White")) %>%
  group_by(race_ethnicity)%>%
  summarise(received=sum(received),
            denied=(sum(denied)))

region_race_21$share_denials_race <- region_race_21$denied/region_race_21$received

race_21_bar <- static_bar_chart(t= region_race_21,
                                x = "share_denials_race",
                                y = "race_ethnicity",
                                fill = "race_ethnicity",
                                title = "Loan Denial Rates by Race and Ethnicity (2021)",
                                subtitle = "Federal Financial Institutions Examination Council (FFIEC), 
Home Mortgage DIsclosure Act (HMDA) Database",
                                             color = "psrc_light")

race_21_bar

# chart #3 ----------------------------------  
region_21 <- region_clean[region_clean$race_ethnicity %in% c("White", "Asian","Black or African American",
                                                             "Hispanic or Latino"), ] %>%
  mutate(median_income = fct_relevel(median_income, 
                                     "less than 50%", "50-79%", "80-99%", "100-119%", "120% or more")) %>%
  group_by(median_income, race_ethnicity)%>%
  summarise(received=sum(received),
            denied=(sum(denied)))

region_21$share_denials_race <- region_21$denied/region_21$received

region_21$data_year <- 2021
region_21$region <- "Region"
region_21 <- region_21[c(6, 7, 1, 2, 3, 4, 5)]

race_income_21_column <- static_column_chart(t= region_21,
                                             x = "median_income",
                                             y = "share_denials_race",
                                             fill = "race_ethnicity",
                                             title = "Loan Denial Rates by Income Bracket and Race/Ethnicity (2021)",
                                             subtitle = "Federal Financial Institutions Examination Council (FFIEC), 
Home Mortgage DIsclosure Act (HMDA) Database",
                                             color = "psrc_light")

race_income_21_column



region_clean_POC <- region_clean %>% 
  mutate(POC = case_when(race_ethnicity == "White" ~"Non POC",
         TRUE ~ "POC"))

# plot data ----------------------------------------- Not currently working

facet_chart <- static_facet_line_chart(t=region_clean,
                        x="data_year", y= "share_denials_race",
                        fill="data_year", facet="median_income",
                        color="psrc_light",
                        est ="percent",
                        scales="fixed")

facet_chart

# Export data

#work_book <- createWorkbook()
#addWorksheet(work_book, sheetName = "median sale price")
#writeData(work_book, "median sale price", county_piv)
#saveWorkbook(work_book, file = "Affordability Measures/r_output median_price_county.xlsx", overwrite = TRUE)
