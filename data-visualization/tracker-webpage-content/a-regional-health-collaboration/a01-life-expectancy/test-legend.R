library(tidyverse)
library(leaflet)
library(sf)
library(psrcplot)
library(magrittr)

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL

  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if(length(bins) > 2) 
        if(!all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      
      if(decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        # "#4A0048" "#808080" "#4D004B" "#7B1779" "#AC5AAA" "#D0A1CF" "#F2CDF7" "#FFFFFF" "#808080" "#FFFFFF"
        # "#4A0048" "#808080" "#4D004B" "#7B1779" "#AC5AAA" "#D0A1CF" "#F2CDF7" "#FFFFFF" "#808080"
        # colors <- pal(rev(c(r[1], cuts, r[2]))) #### Something funny here!!!! ----
        labels <- rev(labFormat(type = "numeric", cuts))
      } else {
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  
  invokeMethod(map, data, "addLegend", legend)
}

# inputs ----

# load map .rda
base_dir <- 'Y:/Equity Indicators/tracker-webpage-content'
theme_dir <- 'a-regional-health-collaboration'
ind_dir <- 'a01-life-expectancy'
rda <- 'a01-life-expectancy-map-data.rda'

load(file = file.path(base_dir,theme_dir,ind_dir,"rda-data",rda))

# set map extent
map.lat <- 47.615
map.lon <- -122.257
map.zoom <- 8.5

# set up palettes
psrc_purple_plus <- c("#FFFFFF", "#FFFFFF", "#F6CEFC", psrc_colors$purples_inc)

psrc_palette <- leaflet::colorNumeric(palette = psrc_purple_plus,
                                      domain = data_tract$estimate)

# set the variable
var_name <- "Life Expectancy"

labels <- sprintf(
  "Census tract: <em>%s</em><br/><strong>%s</strong><br/><body>%s</body><br/><body>%s</bodyD>", data_tract$geoid10, # the code in <> makes the geoid value italic 
  paste("Tract value: ", prettyNum((round(data_tract$estimate, digits=1))), " years"),
  paste("Region value: ", prettyNum((round(data_tract$reg_estimate, digits=1))), " years"),
  paste("County value: ", prettyNum((round(data_tract$cnty_estimate, digits=1))), 
        paste0("(",data_tract$county_name," County)"))) %>% 
  lapply(htmltools::HTML)

# map ----

# map settings
tract_map <- leaflet() %>%
  leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
  leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
  leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                            options = leaflet::leafletOptions(pane = "maplabels"),
                            group = "Labels") %>%
  addPolygons(data = data_tract,
              fillColor = ~psrc_palette(estimate),
              stroke = FALSE, 
              smoothFactor = 0.2,
              fillOpacity = 0.7,
              group = var_name,
              label = labels) %>%

  # legends
  addLegend_decreasing(pal = psrc_palette,
                       values = data_tract$estimate,
                       position = "bottomright",
                       title = var_name,
                       group = var_name,
                       opacity = 0.7,
                       decreasing = TRUE,
                       labFormat = labelFormat()) %>%
  
  #set view extent
  leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom) %>% 
  addEasyButton(easyButton(
    icon = htmltools::span(class = "globe", htmltools::HTML("&#127758;")),  #&#127760; (another emoji option) #"fa-globe", (font awesome icon no longer works because of the conversion to Poppins font below)   
    title ="Region",
    onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }")))

# fix the legend NA placement (https://github.com/rstudio/leaflet/issues/615)
css_fix <- "div.info.legend.leaflet-control br {clear: both;} html * {font-family: Poppins !important;}" # CSS to correct spacing and font family
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
tract_map %<>% htmlwidgets::prependContent(html_fix)

# print map
tract_map