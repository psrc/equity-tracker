path <- "data-visualization\\tracker-webpage-content\\e-housing\\e01-affordable-rent-access\\update"
source(here::here(path, 'insights-analysis-config.R')
       )

middle_year <- 2016
recent_year <- 2021

# middle_year <- 2017
# recent_year <- 2022

load(file = file.path(file_names$base_dir, 
                      file_names$theme_dir,
                      file_names$ind_dir,
                      # "update",
                      "rda-data",
                      file_names$rda_chart))

# Create Facet Column Chart ----

data_clean_column <- data_clean %>%
  filter(data_year == recent_year) #filter on most recent year

# set variables 
chart_vars <- list(
  geo = "county",
  y = "fact_value",
  facet = "focus_type_ord",
  y_min = 0,
  y_max = 1,
  dec = 0,
  esttype = "percent",
  num_colors = 2,
  color_rev = TRUE
)

column_vars <- list(
  df =  data_clean_column,
  x = "focus_attribute_ord",
  fill = "focus_attribute_ord",
  color = "oranges",
  title = "Access to Affordable Rent by Community",
  subtitle = "Percent of census tracts that are affordable",
  source = "U.S. Census Bureau, 2018-2022 American Community Survey 5-Year Public Use Microdata Sample"
)

column_chart <- equity_tracker_column_facet(df = column_vars$df,
                                            geo = chart_vars$geo,
                                            x = column_vars$x,
                                            y = chart_vars$y,
                                            facet = chart_vars$facet,
                                            title = chart_vars$title,
                                            y_min = chart_vars$y_min,
                                            y_max = chart_vars$y_max,
                                            dec = chart_vars$dec,
                                            esttype = chart_vars$esttype,
                                            color = column_vars$color,
                                            num_colors = chart_vars$num_colors,
                                            color_rev = chart_vars$color_rev)


## calculate call out information ----

# focus_type <- unique(data_clean_column$focus_type)

## Older Cat ----
o_df <- data_clean_column %>% 
  filter(focus_type == "Older_cat")

o_df_wide <- o_df |> 
  select(county, focus_attribute_ord, fact_value_notrounded) |> 
  pivot_wider(id_cols = county,
              names_from = focus_attribute_ord,
              values_from = fact_value_notrounded) |> 
  mutate(diff = abs(`Households with\nolder adults` - `Other households`))


# Create Facet Line Chart ----

line_vars <- list(
  df = data_clean,
  x = "data_year", # different from variables for facet column
  fill = "focus_attribute_ord", # different from variables for facet column
  color = "blues",
  title = "Access to Affordable Rent Trend by Community",
  subtitle = "Percent of census tracts that are affordable, in 5-year spans between 2012 and 2022",
  source = "U.S. Census Bureau, 2008-2012, 2013-2017, 2018-2022 American Community Survey 5-Year Public Use Microdata Sample"
)

line_chart <- equity_tracker_line_facet(df = line_vars$df,
                                        geo = chart_vars$geo,
                                        x = line_vars$x,
                                        y = chart_vars$y,
                                        fill = line_vars$fill,
                                        facet = chart_vars$facet,
                                        y_min = chart_vars$y_min,
                                        y_max = chart_vars$y_max,
                                        dec = chart_vars$dec,
                                        esttype = chart_vars$esttype,
                                        color = line_vars$color,
                                        num_colors = chart_vars$num_colors,
                                        color_rev = chart_vars$color_rev,
                                        width = '420px',
                                        height = '380px')
line_chart

## calculate call out information ----

### Biggest disparities ----

# is the gap between two lines (low vulnerability vs high vulnerability) lessening or widening?
disparity_df <- data_clean |> 
  select(data_year, county, vulnerability, focus_type, focus_attribute_ord, fact_value_notrounded) 

disparity_df_wide <- disparity_df |> 
  filter(data_year != middle_year) |> 
  pivot_wider(id_cols = c(data_year, county, focus_type),
              names_from = vulnerability,
              values_from = fact_value_notrounded) |> 
  relocate(`high vulnerability`, .after = `low vulnerability`) |> 
  mutate(disparity = `low vulnerability` - `high vulnerability`) |> 
  group_by(county, focus_type) |> 
  mutate(disparity_btwn_years = disparity-lag(disparity, 1)) |>  # numerator
  mutate(disparity_share = (disparity_btwn_years/lag(disparity, 1)))

disparity_top <- disparity_df_wide |> 
  filter(focus_type != "Youth_cat") |> 
  group_by(data_year, county) |> 
  slice_min(disparity_share, n = 10) |> 
  arrange(data_year, county, disparity_share)

### Deltas ----

delta_df <- data_clean |> 
  select(data_year, county, vulnerability, focus_type, focus_attribute_ord, fact_value_notrounded) |> 
  filter(data_year != middle_year) 

delta_years <- unique(delta_df$data_year)
names(delta_years) <- c("start", "end")

delta_df_wide <- delta_df |> 
  pivot_wider(id_cols = c(county, focus_type, vulnerability),
              names_from = data_year,
              values_from = fact_value_notrounded
              ) |> 
  rename(delta_years) |> 
  mutate(delta = end - start) |> 
  mutate(delta_share = delta/start)

### HH with Lower Income ----

li_dis_df <- disparity_df_wide |> 
  filter(focus_type == "Income_cat", county == "Region")

li_df <- data_clean |> 
  select(data_year, county, vulnerability, focus_type, focus_attribute_ord, aff_tracts, tot_tracts, fact_value_notrounded) |> 
  filter(data_year != middle_year, focus_type == "Income_cat", county == "Region")

# Other households, dropped 12 percentage points. Absolute number/difference for affordable tracts is 28. How does that 
# explain the drop when the denominator (tot tracts) is different across the two years?

### Disability ----

disab_df <- disparity_df_wide |> 
  filter(focus_type == "Disability_cat")

# The disparity in access to affordable rent between those with and without a disability decreased 
# over time for most counties, with the greatest difference in Snohomish (41 percentage points), 
# followed by Kitsap (32 percentage points), and King (14 percentage points). 
# The disparity for Pierce grew by 5 percentage points.

### LEP ----

lep_df <- disparity_df_wide |> 
  filter(focus_type == "LEP_cat")

# The disparity in access to affordable rent between households 
# with limited English proficiency and English proficient households decreased in Snohomish (40 percentage pts), 
# followed by  King (23 percentage points), and Pierce (17 percentage points), while disparity greatly increased in Kitsap. 
# Access to affordable rent in Kitsap dropped nearly 70 percentage points.

lep_delta_df <- delta_df_wide |> 
  filter(focus_type == "LEP_cat")

# The regional share of affordable tracts for renter households with limited English proficiency 
# decreased 80% between 2012 and 2022, a 2x larger decrease than for English proficient renter households (40%).


### Older Adults ----

older_df <- disparity_df_wide |> 
  filter(focus_type == "Older_cat")

### POC ----

delta_df_wide |> 
  filter(focus_type == "POC_cat", county == 'Region')

# 2.5X: The share of tracts with affordable rent decreased 45% for renters of color between 2012 and 2022, 
# about 2.5 times more than the decrease for white non-Hispanic renters (38%)





