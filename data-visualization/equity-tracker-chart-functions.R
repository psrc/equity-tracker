echarts4r::e_common(font_family = "Poppins")

echart_column_chart <- function(df, x, y, facet, geo, title, y_min, y_max, dec, esttype, i, color, width, height) {
  
  max_data <- df %>% dplyr::select(tidyselect::all_of(y)) %>% dplyr::pull() %>% max()
  facet_values <- df %>% dplyr::select(tidyselect::all_of(facet)) %>% dplyr::pull() %>% unique
  num_facets <- df %>% dplyr::select(tidyselect::all_of(facet)) %>% dplyr::distinct() %>% dplyr::pull() %>% length()
  max_year <- df %>% dplyr::select("data_year") %>% dplyr::pull() %>% max()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # If ymax is provided use it, otherwise calculate it
  ifelse(is.null(y_max), max_y <- round(max_data * 1.10,0), max_y <- y_max)
  
  ifelse(i <= 3, top_padding <- 100, top_padding <- 50)
  ifelse(i <= 3, title_padding <- 75, title_padding <- 25)
  ifelse(i <= 3, bottom_padding <- 75, bottom_padding <- 75)
  
  # Create the most basic chart
  c <- df %>%
    dplyr::filter(.data[[facet]] == facet_values[[i]] & .data$data_year == max_year) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::group_by(.data[[geo]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE, elementId = paste0("columnchart",i), width = width, height = height) 
  
  if (color == "blues") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#BFE9E7', '#73CFCB', '#40BDB8', '#00A7A0', '#00716c', '#005753'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "greens") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E2F1CF', '#C0E095', '#A9D46E', '#8CC63E', '#588527', '#3f6618'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "oranges") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#FBD6C9', '#F7A489', '#F4835E', '#F05A28', '#9f3913', '#7a2700'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "purples") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#E3C9E3', '#C388C2', '#AD5CAB', '#91268F', '#630460', '#4a0048'];
                                                               return colorList[params.dataIndex]}"))) 
  }
  
  if (color == "jewel") {
    
    c <- c %>%
      echarts4r::e_bar_(y, 
                        name = title,
                        itemStyle = list(color = htmlwidgets::JS("
                      function(params) {var colorList = ['#91268F', '#F05A28', '#8CC63E', '#00A7A0', '#4C4C4C'];
                                                               return colorList[params.dataIndex]}"))) 
  }

  c <- c %>% 
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_connect(c("columnchart1")) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_title(top = title_padding,
                       left = 'center',
                       textStyle = list(fontSize = 14),
                       text=facet_values[[i]]) 
  
  # Add in the Timeseries Selector to the 2nd chart in the grid
  if(i == 2) {
    
    c <- c %>%
      echarts4r::e_timeline_opts(autoPlay = FALSE,
                                 tooltip = list(show=FALSE),
                                 axis_type = "category",
                                 top = 15,
                                 right = 30,
                                 left = 15,
                                 controlStyle=FALSE,
                                 lineStyle=FALSE,
                                 label = list(show=TRUE,
                                              interval = 0,
                                              color='#4C4C4C',
                                              fontFamily = 'Poppins'),
                                 itemStyle = list(color='#BCBEC0'),
                                 checkpointStyle = list(label = list(show=FALSE),
                                                        color='#4C4C4C',
                                                        animation = FALSE),
                                 progress = list(label = list(show=FALSE),
                                                 itemStyle = list (color='#BCBEC0')),
                                 emphasis = list(label = list(show=FALSE),
                                                 itemStyle = list (color='#4C4C4C')))
  } else {
    
    c <- c %>% echarts4r::e_timeline_opts(show = FALSE)
    
  }
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec), min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.marker + ' ' +\n
      params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD"), min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.marker + ' ' +\n
      params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_y_axis(min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}

echart_line_chart <- function(df, x, y, facet, geo, title, y_min, y_max, dec, esttype, i, color, width, height, fill) {
  
  if (color == "blues") {chart_color <- psrcplot::psrc_colors$blues_inc}
  if (color == "greens") {chart_color <- psrcplot::psrc_colors$greens_inc}
  if (color == "oranges") {chart_color <- psrcplot::psrc_colors$oranges_inc}
  if (color == "purples") {chart_color <- psrcplot::psrc_colors$purples_inc}
  if (color == "jewel") {chart_color <- psrcplot::psrc_colors$pognbgy_5}
  
  max_data <- df %>% dplyr::select(tidyselect::all_of(y)) %>% dplyr::pull() %>% max()
  facet_values <- df %>% dplyr::select(tidyselect::all_of(facet)) %>% dplyr::pull() %>% unique
  num_facets <- df %>% dplyr::select(tidyselect::all_of(facet)) %>% dplyr::distinct() %>% dplyr::pull() %>% length()
  line_values <- df %>% 
    dplyr::select(tidyselect::all_of(facet), tidyselect::all_of(fill)) %>% 
    dplyr::filter(.data[[facet]] == facet_values[[i]]) %>%
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    unique
  
  chart_fill <- as.character(line_values)
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # If ymax is provided use it, otherwise calculate it
  ifelse(is.null(y_max), max_y <- round(max_data * 1.10,0), max_y <- y_max)
  
  # The grid spacing differs for the top 3 versus bottom 3 charts
  ifelse(i <= 3, top_padding <- 100, top_padding <- 50)
  ifelse(i <= 3, title_padding <- 75, title_padding <- 25)
  ifelse(i <= 3, bottom_padding <- 75, bottom_padding <- 75)
  
  # Create the Basic Chart
  chart_df <- df %>%
    dplyr::filter(.data[[facet]] == facet_values[[i]]) %>%
    dplyr::filter(.data[[fill]] %in% chart_fill) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::select(tidyselect::all_of(facet), tidyselect::all_of(fill), 
                  tidyselect::all_of(x), tidyselect::all_of(y), tidyselect::all_of(geo)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(fill), values_from = tidyselect::all_of(y))
  
  c <- chart_df %>%
    dplyr::group_by(.data[[geo]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE, elementId = paste0("linechart",i), width = width, height = height)
  
  for(fill_items in chart_fill) {
    c <- c %>%
      echarts4r::e_line_(fill_items, smooth = FALSE)
  }
  
  c <- c %>% 
    echarts4r::e_color(chart_color) %>%
    echarts4r::e_legend(show = TRUE, bottom=0) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_connect(c("linechart1")) %>%
    echarts4r::e_x_axis(axisTick=list(show = TRUE,
                                      alignWithLabel = TRUE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_title(top = title_padding,
                       left = 'center',
                       textStyle = list(fontSize = 14),
                       text=facet_values[[i]]) 
  
  # Add in the Timeseries Selector to the 2nd chart in the grid
  if(i == 2) {
    
    c <- c %>%
      echarts4r::e_timeline_opts(autoPlay = FALSE,
                                 tooltip = list(show=FALSE),
                                 axis_type = "category",
                                 top = 15,
                                 right = 30,
                                 left = 15,
                                 controlStyle=FALSE,
                                 lineStyle=FALSE,
                                 label = list(show=TRUE,
                                              interval = 0,
                                              color='#4C4C4C',
                                              fontFamily = 'Poppins'),
                                 itemStyle = list(color='#BCBEC0'),
                                 checkpointStyle = list(color='#4C4C4C'),
                                 progress = list(itemStyle = list (color='#BCBEC0')),
                                 emphasis = list(itemStyle = list (color='#4C4C4C')))
  } else {
    
    c <- c %>% echarts4r::e_timeline_opts(show = FALSE)
    
  }
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec), min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
    function(params, ticket, callback) {
    var fmt = new Intl.NumberFormat('en',
    {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
    var idx = 0;\n
    if (params.name == params.value[0]) {\n
    idx = 1;\n        }\n
    return(params.marker + ' ' +\n
    params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
    )
    }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD"), min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
    function(params, ticket, callback) {
    var fmt = new Intl.NumberFormat('en',
    {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
    var idx = 0;\n
    if (params.name == params.value[0]) {\n
    idx = 1;\n        }\n
    return(params.marker + ' ' +\n
    params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
    )
    }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_y_axis(min=y_min, max=max_y) %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}

equity_arrange <- function(charts, rows = NULL, cols = NULL, width = "xs", title = NULL) {
  
  plots <- charts
  
  if (is.null(rows)) {
    rows <- length(plots)
  }
  
  if (is.null(cols)) {
    cols <- 1
  }
  
  w <- "-xs"
  if (!isTRUE(getOption("knitr.in.progress"))) {
    w <- ""
  }
  
  x <- 0
  tg <- htmltools::tagList()
  for (i in 1:rows) {
    r <- htmltools::div(class = "row")
    
    for (j in 1:cols) {
      ifelse(j==2, z<-2, z<-1)
      x <- x + 1
      cl <- paste0("col", w, "-", 12 / cols)
      if (x <= length(plots)) {
        c <- htmltools::div(class = cl, plots[[x]], style = {paste0("z-index: ",z,";")})
      } else {
        c <- htmltools::div(class = cl)
      }
      r <- htmltools::tagAppendChild(r, c)
    }
    tg <- htmltools::tagAppendChild(tg, r)
  }
  
  if (!isTRUE(getOption("knitr.in.progress"))) {
    htmltools::browsable(
      htmltools::div(
        class = "container-fluid",
        htmltools::tags$head(
          htmltools::tags$link(
            rel = "stylesheet",
            href = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css",
            integrity = "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO",
            crossorigin = "anonymous"
          )
        ),
        htmltools::h3(title),
        tg
      )
    )
  } else {
    if (!is.null(title)) {
      htmltools::div(title, tg)
    } else {
      tg
    }
  }
}

equity_tracker_column_facet <- function(df, x, y, facet, geo, title, y_min=0, y_max=NULL, dec=0, esttype="number", color="blues", width = '420px', height = '380px', r=2, c=3) {
  
  num_facets <- seq(1, df %>% select(all_of(facet)) %>% distinct() %>% pull() %>% length(), by=1)
  
  create_charts <- partial(echart_column_chart, 
                           df = df,
                           geo = geo,
                           x = x,
                           y = y, 
                           facet = facet,
                           title = title,
                           dec = dec, 
                           y_min = y_min, 
                           y_max = y_max,
                           esttype = esttype, 
                           color = color,
                           width = width, 
                           height = height)
  
  plots <- map(num_facets,create_charts)
  
  chart <- equity_arrange(charts=plots, rows=r, cols=c)
  
  return(chart)
  
}

equity_tracker_line_facet <- function(df, x, y, facet, geo, title, y_min=0, y_max=NULL, dec=0, esttype="number", color="blues", width = '420px', height = '380px', r=2, c=3, fill) {
  
  num_facets <- seq(1, df %>% select(all_of(facet)) %>% distinct() %>% pull() %>% length(), by=1)
  
  create_charts <- partial(echart_line_chart, 
                           df = df,
                           geo = geo,
                           x = x,
                           y = y, 
                           facet = facet,
                           title = title,
                           dec = dec, 
                           y_min = y_min, 
                           y_max = y_max,
                           esttype = esttype, 
                           color = color,
                           width = width, 
                           height = height,
                           fill=fill)
  
  plots <- map(num_facets,create_charts)
  
  chart <- equity_arrange(charts=plots, rows=r, cols=c)
  
  return(chart)
  
}
