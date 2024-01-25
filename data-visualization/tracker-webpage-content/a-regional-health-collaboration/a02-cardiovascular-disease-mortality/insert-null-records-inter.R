library(psrcplot)
library(tidyverse)

base_dir <- 'Y:/Equity Indicators/tracker-webpage-content'
theme_dir <- 'a-regional-health-collaboration'
ind_dir <- 'a02-cardiovascular-disease-mortality'

source('C:\\Users\\CLam\\github\\equity-tracker\\data-visualization\\equity-tracker-chart-functions.R') # this needs to be adjusted based on your local GitHub directory - it is required if you wish to run chunks and view the visuals (without first knitting)

# set folder structure
rda <- 'a02-cardiovascular-disease-mortality-data.rda'

load(file = file.path(base_dir, theme_dir, ind_dir, "rda-data", rda))

# insert null records ----
standard_base_year <- 2010

## main table
df_base <- data_clean %>% 
  mutate(data_year_num = as.numeric(data_year))

## set-up columns to identify missing years by the unique categories/geography
d <- df_base %>% 
  group_by(county, equity_group, quintile, county_ord, equity_group_ord, quintile_ord) %>% 
  summarise(avail_years = list(unique(as.numeric(data_year))),
            num_unique_years = length(unique(data_year)),
            start_year = as.numeric(min(unique(data_year))),
            end_year = as.numeric(max(unique(data_year)))) %>% 
  mutate(base_year = standard_base_year)

## create list-column, a vector of missing years in a cell
d_calc <- d %>% 
  rowwise() %>% 
  mutate(missing_years = list(setdiff(seq(base_year, end_year), avail_years)))

## unpack so each missing year is a row of its own
d_unnest <- d_calc %>% 
  unnest(missing_years) 

## create table with null records for chart
df_nulls <- d_unnest %>% 
  dplyr::select(county, equity_group, quintile, county_ord, equity_group_ord, quintile_ord, data_year_num = missing_years) %>% 
  mutate(wt_combo_rate = NA, estimate = NA, data_year = as.character(data_year_num))

## assemble main table  
df <- bind_rows(df_base, df_nulls) %>% 
  arrange(county_ord, equity_group_ord, quintile_ord, data_year_num)

# filter for relevant years
df2 <- df %>% 
  filter(data_year_num %in% c(2010, 2015, 2020))

# Create Facet Line Chart
# set variables
df = df2 #data_clean
geo = "county_ord"
y = "estimate"
x = "data_year" # different from variables for facet column
fill = "quintile_ord" # different from variables for facet column
y_min = 0
y_max = 400
color = "purples"
facet = "equity_group_ord"
dec = 1
esttype = "number"
num_colors = 5
color_rev = FALSE
title = "Cardiovascular Disease Mortality"
subtitle = "Age adjusted rate per 100,000"
source = paste("Washington Tracking Network, Washington State Department of Health,", "U.S. Census Bureau, American Community Survey (ACS) 2020 5-Year Public Use Microdata Sample (PUMS)", sep="\n")



line_chart <- equity_tracker_line_facet(df = df,
                                        geo = geo,
                                        x = x,
                                        y = y,
                                        fill = fill,
                                        facet = facet,
                                        y_min = y_min,
                                        y_max = y_max,
                                        dec = dec,
                                        esttype = esttype,
                                        color = color,
                                        num_colors = num_colors,
                                        color_rev = color_rev,
                                        width = '420px',
                                        height = '380px')

line_chart
