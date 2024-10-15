library(tidyverse)
library(leaflet)
library(psrcplot)
library(sf)
library(here)
library(magrittr)

source(here("data-visualization", "addLegend-dec.R"))
source(here("data-visualization", 'equity-tracker-chart-functions.R'))

create_map <- function(acs_data_tract, psrc_colors) {
  # setting map extent
  map.lat<- 47.615
  map.lon<- -122.257
  map.zoom<- 8.5
  
  # setting color palette
  psrc_purple_plus<-c("#FFFFFF", "#FFFFFF", "#F6CEFC", psrc_colors$purples_inc)
  
  psrc_palette <- leaflet::colorFactor(palette = psrc_colors$purples_inc,
                                       domain = acs_data_tract$affordability)
  
  # setting the legend/label variables
  var_name <- "Affordability" # the <br> could be adjusted depending on the length/spacing of the name
  
  labels <- sprintf(
    "Census tract: <em>%s</em><br/><strong>%s</strong><br/><body>%s</body><br/><body>%s</body><br/><body>%s</body><br/><body>%s</bodyD>",
    acs_data_tract$geoid20, # the code in <> makes the geoid value bold/italic
    paste("Tract value: ", acs_data_tract$affordability),
    paste("Tract data reliability: ", acs_data_tract$reliability),
    paste("Regional monthly median income: ","$",
          prettyNum((round(acs_data_tract$reg_med_income_monthly, digits=-1)),
                    big.mark = ",")),
    paste("Regional affordability threshold*: ","$",
          prettyNum((round(acs_data_tract$income_30perc, digits=-1)),
                    big.mark = ",")),
    paste("Tract Median Gross Rent: ","$",
          prettyNum((round(acs_data_tract$estimate, digits=-1)),
                    big.mark = ","))) %>%
    lapply(htmltools::HTML)
  
  # map settings
  acs_map <- leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                              options = leaflet::leafletOptions(pane = "maplabels"),
                              group = "Labels") %>%
    addPolygons(data = acs_data_tract,
                fillColor = psrc_palette(acs_data_tract$affordability),
                stroke = TRUE, 
                smoothFactor = 0.2,
                fillOpacity = 0.7,
                group = var_name,
                label = labels) %>% 
    
    # legend
    addLegend_decreasing(pal = psrc_palette,
                         values = acs_data_tract$affordability,
                         position = "bottomright",
                         title = var_name,
                         group = var_name,
                         opacity = 0.7,
                         labFormat = labelFormat(),
                         decreasing = FALSE) %>% #to get legend high-low with correct color order
    
    # set view extent
    leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom) %>% 
    addEasyButton(easyButton(
      icon = htmltools::span(class = "globe", htmltools::HTML("&#127758;")),  #&#127760; (another emoji option) #"fa-globe", (font awesome icon no longer works because of the conversion to Poppins font below)  
      title ="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }")))
  
  # fixing the legend NA placement (https://github.com/rstudio/leaflet/issues/615)
  css_fix <- "div.info.legend.leaflet-control br {clear: both;} html * {font-family: Poppins !important;}" # CSS to correct spacing and font family
  html_fix <- htmltools::tags$style(type = "text/css", css_fix) # Convert CSS to HTML
  acs_map %<>% htmlwidgets::prependContent(html_fix) # Insert into leaflet HTML code
  
  return(acs_map)
}

file_names <- list(base_dir = 'Y:/Equity Indicators/tracker-webpage-content',
                   theme_dir = 'e-housing',
                   ind_dir = 'e01-affordable-rent-access',
                   rda_map = 'e01-affordable-rent-access-map-data.rda',
                   map_name = 'e01-affordable-rent-access-map.html',
                   rda_chart = 'e01-affordable-rent-access.rda',
                   chart_column = 'e01-affordable-rent-access-column',
                   chart_line = 'e01-affordable-rent-access-line')