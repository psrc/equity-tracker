source('insights-analysis-config.R')

# map ----
# load map rda
load(file = file.path(file_names$base_dir,
                      file_names$theme_dir,
                      file_names$ind_dir,
                      "update",
                      "rda-data",
                      file_names$rda_map))

# View top 10 tracts with highest median gross rent ----

# highest_mgr_2021 <- acs_data_tract |>
#   arrange(desc(estimate)) |> 
#   head(10)
# 
# highest_mgr_map_2021 <- create_map(highest_mgr_2021, psrc_colors)
# highest_mgr_map_2021

highest_mgr <- acs_data_tract |>
  arrange(desc(estimate)) |>
  head(10)

highest_mgr_map <- create_map(highest_mgr, psrc_colors)
highest_mgr_map
# Ten census tracts tied with the highest median gross rent ($3,500) are all in King County: 
# 1 is in Seattle (Montlake, south of UW)
# 2 are in Mercer Island, 
# 2 are in Clyde Hill/Hunts Point/Medina/Yarrow Point, 
# 4 are in Bellevue
# 1 in Auburn (West Hill)

# The highest median gross rent in Pierce, Kitsap, and Snohomish County

highest_mgr_cnty <- acs_data_tract |>
  group_by(county_name) |> 
  slice_max(order_by = estimate, n = 2) |> 
  filter(county_name != "King") |> 
  arrange(county_name, desc(estimate))

highest_mgr_map_kit <- create_map(highest_mgr_cnty |> filter(county_name == 'Kitsap'), psrc_colors)
highest_mgr_map_prc <- create_map(highest_mgr_cnty|> filter(county_name == 'Pierce'), psrc_colors)
highest_mgr_map_sno <- create_map(highest_mgr_cnty|> filter(county_name == 'Snohomish'), psrc_colors)

highest_mgr_map_kit
highest_mgr_map_prc
highest_mgr_map_sno

# * The highest median gross rent in Pierce, Kitsap, and Snohomish County are in: 
# Tehaleh community south of Bonney Lake (\$3,090), 
# the southeastern portion of Bainbridge Island (\$2,720), 
# and the Edmonds/southern Mukilteo area (\$3,280), respectively

# Relatively affordable areas are in

