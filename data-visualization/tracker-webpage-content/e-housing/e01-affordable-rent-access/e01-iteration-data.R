library(tidyverse)
library(psrcelmer)
library(psrccensus)

# set indicator information
indicator_measure <- 'median_household_income'
indicator_title <- 'Access to Affordable Rent'

# PUMS/OSPI data set
pums_ospi_elmer <- get_table(schema="equity", tbl_name="v_tracker_indicators")

# clean data set
data_full <- pums_ospi_elmer %>% 
  # distinct(indicator_fact_id, .keep_all = TRUE) %>% #this may be necessary depending on indicator
  dplyr::filter(indicator_type==indicator_measure) %>% 
  dplyr::filter(focus_type!="Total") %>% 
  filter(focus_attribute!="Total") %>%
  filter(indicator_attribute!="Total") %>%
  dplyr::mutate(data_year_yr=format(data_year,format="%Y"))

# wrap/order labels ----
county_order <- c("Region", "King", "Kitsap", "Pierce", "Snohomish")

focus_type_order <- c("People of Color", 
                      "Households with Lower Income", 
                      "People with a Disability", 
                      "Households with Limited English Proficiency", 
                      "Households with Youth <18", 
                      "Households with Older Adults 65+")

focus_attribute_order <- c("People of color",
                           "White\nnon-Hispanic",
                           "Households with\nlower income",
                           "With a\ndisability",
                           "Without a\ndisability",
                           "Limited English\nproficiency",
                           "English\nproficient",
                           "Households with\nyouth",
                           "Households with\nolder adults",
                           "Other households")

# transforming data labels ----
data_clean <- data_full %>% 
  mutate(county = factor(county, levels=county_order)) %>%
  mutate(focus_type_ord = case_when(
    focus_type=="POC_cat"~"People of Color",
    focus_type=="Disability_cat"~"People with a Disability",
    focus_type=="LEP_cat"~"Households with Limited English Proficiency",
    focus_type=="Income_cat"~"Households with Lower Income",
    focus_type=="Youth_cat"~"Households with Youth <18",
    focus_type=="Older_cat"~"Households with Older Adults 65+")) %>%
  mutate(focus_type_ord = factor(focus_type_ord, levels = focus_type_order)) %>%
  mutate(focus_attribute_ord = case_when(
    focus_attribute== "POC"~ "People of color",
    focus_attribute== "Non-POC"~ "White non-Hispanic",
    focus_attribute== "Low Income"~ "Households with lower income",
    focus_attribute== "Non-Low Income"~ "Other households",
    focus_attribute== "With disability"~ "With a disability",
    focus_attribute== "Without disability"~ "Without a disability",
    focus_attribute== "Limited English proficiency"~ "Limited English proficiency",
    focus_attribute== "English proficient"~ "English proficient",
    focus_attribute== "Household with youth"~ "Households with youth",
    focus_attribute== "Household without youth"~ "Other households",
    focus_attribute== "Household with older adult"~ "Households with older adults",
    focus_attribute== "Household without older adult"~ "Other households")) %>% 
  mutate(focus_attribute_ord = str_wrap(focus_attribute_ord, width=16)) %>%
  mutate(focus_attribute_ord = factor(focus_attribute_ord, levels = focus_attribute_order)) 

# echarts wants axis to be factors/characters so convert year to a character
data_clean$data_year <- as.character(data_clean$data_year)

# Sort the data to ensure the charts work
data_clean <- data_clean %>%
  arrange(county, focus_type_ord, focus_attribute_ord, data_year)

## 3. Calculate affordability with median gross rent
### Monthly, 30% of income (equity group)
data_clean_affordability <- data_clean %>% 
  dplyr::mutate(income_per_month = fact_value/12,
                income_30perc = income_per_month * 0.3)

### Median gross rent (tract level) ----
# set variable for same years as in PUMS dataset
years_of_interest <- c(as.numeric(unique(data_clean$data_year_yr)))

# getting median gross rent data by tract - ACS
base_acs_data <- get_acs_recs(geography ='tract', 
                              table.names = 'B25064',
                              years = years_of_interest,
                              acs.type = 'acs5')

acs_data <- base_acs_data %>% 
  dplyr::mutate(county_code=substr(GEOID, 3, 5), # add county to tract level data
                county = case_when(county_code=="033" ~"King",
                                   county_code=="035" ~"Kitsap",
                                   county_code=="053" ~"Pierce",
                                   county_code=="061" ~"Snohomish")) %>% 
  dplyr::select(GEOID, estimate, year, county)