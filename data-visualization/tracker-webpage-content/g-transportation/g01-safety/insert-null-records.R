library(tidyverse)

source('C:\\Users\\CLam\\github\\equity-tracker\\data-visualization\\equity-tracker-chart-functions.R')

# inputs ----

base_dir <- 'Y:/Equity Indicators/tracker-webpage-content'
theme_dir <- 'g-transportation'
ind_dir <- 'g01-safety'

rda <- 'g01-safety-data.rda'
load(file = file.path(base_dir, theme_dir, ind_dir, "rda-data", rda))

# insert null records ----
standard_base_year <- 2010

# main table
df_base <- data_clean %>% 
  mutate(data_year_num = as.numeric(data_year))

# set-up columns to identify missing years by the unique categories/geography
d <- df_base %>% 
  group_by(county, equity_group, quintile, county_ord, equity_group_ord, quintile_ord) %>% 
  summarise(num_unique_years = length(unique(data_year)),
            start_year = as.numeric(min(unique(data_year))),
            end_year = as.numeric(max(unique(data_year)))) %>% 
  mutate(base_year = standard_base_year)

# create list-column, a vector of missing years in a cell
d_calc <- d %>% 
  rowwise() %>% 
  mutate(missing_years = ifelse(start_year == base_year, NA, list(seq(base_year, (start_year-1))))) 

# unpack so each missing year is a row of its own
d_unnest <- d_calc %>% 
  unnest(missing_years) 

# create table with null records for chart
df_nulls <- d_unnest %>% 
  select(county, equity_group, quintile, county_ord, equity_group_ord, quintile_ord, data_year_num = missing_years) %>% 
  mutate(wt_combo_rate = NA, estimate = NA, data_year = as.character(data_year_num))

# assemble main table  
df <- bind_rows(df_base, df_nulls) %>% 
  arrange(county_ord, equity_group_ord, data_year_num)
 

# prep line chart ----

## set variables

x <- "data_year" 
y <- "estimate"
y_min <- 0
y_max <- 100

geo <- "county_ord"
fill <- "quintile_ord" 
facet <- "equity_group_ord"

dec <- 1
esttype <- "number"

color <- "purples"
num_colors <- 5
color_rev <- FALSE

title <- "Traffic Related Deaths and <br> Serious Injuries per 100,000"
subtitle <- "Census Tract Level"
source <- paste("Washington Traffic Safety Commission", 
               "U.S. Census Bureau, American Community Survey (ACS) 2020 5-Year Public Use Microdata Sample (PUMS)", sep="\n")

## create chart
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
