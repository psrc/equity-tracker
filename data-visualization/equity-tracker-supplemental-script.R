# This script is meant to streamline the data-gen and data-vis scripts for each indicator. 
# This script will include the common/repetitive code that is used across the indicators

# FORMATTING --------
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

transform_data_labels <- function(table) {
  data_clean <- table |>  
    mutate(county = factor(county, levels=county_order)) |>
    mutate(focus_type_ord = case_when(
      focus_type =="POC_cat"~"People of Color",
      focus_type =="Disability_cat"~"People with a Disability",
      focus_type =="LEP_cat"~"Households with Limited English Proficiency",
      focus_type =="Income_cat"~"Households with Lower Income",
      focus_type =="Youth_cat"~"Households with Youth <18",
      focus_type =="Older_cat"~"Households with Older Adults 65+")) |>
    mutate(focus_type_ord = factor(focus_type_ord, levels = focus_type_order)) |>
    mutate(focus_attribute_ord = case_when(
      focus_attribute == "POC"~ "People of color",
      focus_attribute == "Non-POC"~ "White non-Hispanic",
      focus_attribute == "Low Income"~ "Households with lower income",
      focus_attribute == "Non-Low Income"~ "Other households",
      focus_attribute == "With disability"~ "With a disability",
      focus_attribute == "Without disability"~ "Without a disability",
      focus_attribute == "Limited English proficiency"~ "Limited English proficiency",
      focus_attribute == "English proficient"~ "English proficient",
      focus_attribute == "Household with youth"~ "Households with youth",
      focus_attribute == "Household without youth"~ "Other households",
      focus_attribute == "Household with older adult"~ "Households with older adults",
      focus_attribute == "Household without older adult"~ "Other households")) |> 
    mutate(focus_attribute_ord = str_wrap(focus_attribute_ord, width = 16)) |>
    mutate(focus_attribute_ord = factor(focus_attribute_ord, levels = focus_attribute_order)) 
  
  # echarts wants axis to be factors/characters so convert year to a character
  data_clean$data_year <- as.character(data_clean$data_year)
  
  # Sort the data to ensure the charts work
  data_clean <- data_clean |>
    arrange(county, focus_type_ord, focus_attribute_ord, data_year)
  
  return(data_clean)
}



# MAP SETTINGS --------
## This code helps to set up the legend so that it is arranged high-low with correct color order (https://stackoverflow.com/questions/40276569/reverse-order-in-r-leaflet-continuous-legend) 
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
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
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


## set map extent
map.lat<- 47.615
map.lon<- -122.257
map.zoom<- 8.5