setwd("C:\\Users\\CLam\\github\\equity-tracker\\data-visualization\\tracker-webpage-content\\e-housing\\e01-affordable-rent-access\\")
source("e01-iteration-data.R")

# Df of total # tracts by year
num_tracts_region <- acs_data %>%
  group_by(year) %>% 
  summarise(tot_tracts = n())

num_tracts_counties <- acs_data %>% 
  group_by(year, county) %>% 
  summarise(tot_tracts = n())

# Calculate number of affordable tracts ---
geographies <- as.character(unique(data_clean$county))
equity_groups <- unique(data_clean$focus_attribute)

# initiate master list, will contain all dfs
all_dfs <- list()

for(y in 1:length(years_of_interest)) {
  for(g in 1:length(geographies)) {
    
    a_df <- NULL # a placeholder for a df single year by one geography with all equity groups
    
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
      
      # Consolidate all equity groups for a single year by geography
      if(is.null(a_df)) {
        a_df <- tracts
      } else {
        a_df <- a_df %>% 
          left_join(tracts, by = c('GEOID', 'estimate', 'year', 'county'))
      }
      
    } # end equity groups
    
    # add to master list as element 
    all_dfs[[paste0(geographies[g], "_", years_of_interest[y])]] <- a_df
  
    } # end geographies
} # end years

reg_dfs_names <- paste0("Region_", years_of_interest)
co_dfs_names <- names(all_dfs[!(names(all_dfs) %in% reg_dfs_names)])

# create a table for regional tallies and a separate one for individual counties
regional_df <- data.table::rbindlist(all_dfs[reg_dfs_names]) 
counties_df <- data.table::rbindlist(all_dfs[co_dfs_names])

# regional tally
r_cols <- colnames(regional_df)[!(colnames(regional_df) %in% c('GEOID', 'estimate', 'year', 'county'))]

region_tally <- regional_df %>% 
  group_by(year) %>% 
  summarise(across(all_of(r_cols), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = all_of(r_cols),
               names_to = "equity_group",
               values_to = "aff_tracts") %>% 
  left_join(num_tracts_region, by = "year") %>% 
  mutate(share_aff_tracts = aff_tracts/tot_tracts,
         county = 'Region')

# counties tally
counties_tally <- counties_df %>% 
  group_by(year, county) %>% 
  summarise(across(all_of(r_cols), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = all_of(r_cols),
               names_to = "equity_group",
               values_to = "aff_tracts") %>% 
  left_join(num_tracts_counties, by = c("year", "county")) %>% 
  mutate(share_aff_tracts = aff_tracts/tot_tracts)

# bind tallies & merge with main table
tally <- bind_rows(region_tally, counties_tally) %>% 
  mutate(data_year = as.character(year),
         focus_attribute = str_extract(equity_group, "^.*(?=_)")) %>% 
  select(-year, -equity_group)

final_df <- data_clean_affordability %>% 
  left_join(tally, by = c('data_year', 'county', 'focus_attribute'))
