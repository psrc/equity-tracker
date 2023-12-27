setwd("C:\\Users\\CLam\\github\\equity-tracker\\data-visualization\\tracker-webpage-content\\e-housing\\e01-affordable-rent-access\\")
source("e01-iteration-data.R")

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

# List of total # tracts by year
num_tracts_region <- acs_data %>%
  group_by(year) %>% 
  summarise(tot_tracts = n())

### Calculate number of affordable tracts ---
geographies <- as.character(unique(data_clean$county))
equity_groups <- unique(data_clean$focus_attribute)

all_dfs <- list()

for(y in 1:length(years_of_interest)) {
  for(g in 1:length(geographies)) {
    
    same_geog_year_dfs <- NULL
    
    for(eg in 1:length(equity_groups)) {
      
      # equity value (# to measure against each tract)
      eq_df <- data_clean_affordability %>%
        filter(county == geographies[g] & 
                 data_year == years_of_interest[y] &
                 focus_attribute == equity_groups[eg])
      
      if(geographies[g] == 'Region') {
        tracts <- acs_data %>%
          filter(year == years_of_interest[y]) 
      } else {
        tracts <- acs_data %>%
          filter(year == years_of_interest[y] & county == geographies[g]) 
      }
      
      tracts <- tracts %>% 
        dplyr::mutate("{equity_groups[eg]}_afford" := case_when(estimate < eq_df$income_30perc ~ 1,
                                                                estimate > eq_df$income_30perc ~ 0,
                                                                TRUE ~ NA))   
      
      # Consolidate all equity groups for single year & geography
      if(is.null(same_geog_year_dfs)) {
        same_geog_year_dfs <- tracts
      } else {
        same_geog_year_dfs <- same_geog_year_dfs %>% 
          left_join(tracts, by = c('GEOID', 'estimate', 'year', 'county'))
      }
      
    } # end equity groups
    
    # add to master list as element 
    all_dfs[[paste0(geographies[g], "_", years_of_interest[y])]] <- same_geog_year_dfs
  
    } # end geographies
}

region <- all_dfs[paste0("Region_", years_of_interest)]


# QC script ----
# all_dfs$Region_2011 %>% 
#   group_by(POC_afford) %>% 
#   summarise(count = n())
# 
# all_dfs$King_2011 %>% 
#   group_by(POC_afford) %>% 
#   summarise(count = n())





# # regional numbers - function

# data_clean_affordability_2021 <- data_clean_affordability %>%
#   dplyr::filter(data_year_yr=="2021")
# 
# acs_data_2021 <- acs_data %>%
#   dplyr::filter(year==2021)
# 
# region_function <- function(equity_df, geo_df, geography, equity_group){
# 
#   equity_df2 <- equity_df %>%
#     dplyr::filter(county==geography,
#                   focus_attribute==equity_group)
# 
#   final_df <- geo_df %>%
#     dplyr::mutate("{equity_group}_afford" := case_when(estimate<equity_df2$income_30perc~1,
#                                                        estimate>equity_df2$income_30perc~0,
#                                                        TRUE~NA))
#   return(final_df)
# }
# 
# # generate equity group data
# poc_region <- region_function(data_clean_affordability_2021, acs_data_2021, "Region", "POC")
# non_poc_region <- region_function(data_clean_affordability_2021, acs_data_2021, "Region", "Non-POC")
# low_income_region <- region_function(data_clean_affordability_2021, acs_data_2021, "Region", "Low Income")
# non_low_income_region <- region_function(data_clean_affordability_2021, acs_data_2021, "Region", "Non-Low Income")

# # combine all equity group data
# test_bind <- mutate(poc_region, non_poc_region, low_income_region, non_low_income_region)
# 
# # calculate the number/share of affordable tracts
# test_sum_region <- test_bind %>% 
#   dplyr::group_by(year) %>% 
#   dplyr::summarize(POC=sum(POC_afford, na.rm=TRUE),
#                    `Non-POC`=sum(`Non-POC_afford`, na.rm=TRUE),
#                    `Low Income`=sum(`Low Income_afford`, na.rm=TRUE),
#                    `Non-Low Income`=sum(`Non-Low Income_afford`, na.rm=TRUE)) %>% 
#   pivot_longer(cols= c(POC,`Non-POC`,`Low Income`,`Non-Low Income`), 
#                names_to = "equity_group", 
#                values_to = "affordable_tracts") %>% 
#   dplyr::mutate(county="Region",
#                 share_affordable_tracts=affordable_tracts/nrow(acs_data_2021))
# 
# # join to regional equity data
# head(data_clean_affordability_2021)
# 
# test_merge <- merge(data_clean_affordability_2021, test_sum_region,
#                     by.x=c("data_year", "county", "focus_attribute"),
#                     by.y=c("year", "county", "equity_group"),
#                     all.x=TRUE)