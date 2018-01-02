
shinyServer(function(input, output, session) {
  
  # Reactive ----------------------------------------------------------------
  
  # incidents statistics
  incidentsStats <- reactiveValues()
  
  # Filter for Perpetrator groups
  filterPerpetrators_rows <- eventReactive(input$filterPerpetrator, {
    if (length(input$filterPerpetrator) > 0) {
      which(df$Perpetrator %in% input$filterPerpetrator)
    } else {
      return(1:nrow(df))
    }
  })
  
  # Observe -----------------------------------------------------------------
  
  # Hightlight the incident selected in the timeline via leaflet popup
  
  observeEvent(input$myTimeline_selected,{
    
    df.highlight <- df[as.numeric(input$myTimeline_selected),]
    
    leafletProxy("mymap", data = df.highlight) %>%
      clearPopups() %>%
      addPopups(lng = ~lon, lat = ~lat, popup = ~paste(myPopup),
                options = popupOptions(closeOnClick = TRUE))
    
  })
  
  # Hightlight the incident selected in the map via leaflet popup
  
  observeEvent(input$mymap_marker_click,{
    
    df.highlight <- df[(input$mymap_marker_click$id),]
    
    leafletProxy("mymap", data = df.highlight) %>%
      clearPopups() %>% 
      addPopups(lng = ~lon, lat = ~lat, popup = ~paste(myPopup),
                options = popupOptions(closeOnClick = TRUE))
  })
  
  # Update Incidents stats
  
  observe({ 
    
    # default if no perpetrator filter is set
    if (length(input$filterPerpetrator) == 0) {
      df.table <- df
    } else {
      df.table <- df[filterPerpetrators_rows(), ]
    }
    
    # filter by date
    df.table <- df.table %>% 
      filter(Date >= input$filterDate[1] & Date <= input$filterDate[2]) %>% 
      summarize(KPI.incidents = n(),
                KPI.injured = sum(Injured),
                KPI.fatalities = sum(Dead))
    
    stats <- data.frame(feature = c("Incidents:", "Injured:", "Fatalities:"),
               values = prettyNum(c(df.table$KPI.incidents, df.table$KPI.injured, df.table$KPI.fatalities), big.mark = ".", decimal.mark = ","))
    
    incidentsStats$Incidents <- stats$values[1]
    incidentsStats$Injured <- stats$values[2]
    incidentsStats$Fatalities <- stats$values[3]
  })

  # Leaflet -----------------------------------------------------------------

  df$color <- df$Perpetrator
  factpal <- colorFactor(rev(myColors), domain = levels(df$color), ordered = FALSE)
  
  output$mymap <- renderLeaflet({
    
      mapbox <- paste0("https://api.mapbox.com/styles/v1/datascience42/cjboxebfq6diw2rs7x1d1cpqj/tiles/256/{z}/{x}/{y}?access_token=", MAPBOX_KEY)
      mapbox_attribution <- "<a href='http://mapbox.com/about/maps' class='mapbox-wordmark' target='_blank'>Mapbox</a>
      <a href='https://www.mapbox.com/about/maps/'>© Mapbox</a>
      <a href='http://www.openstreetmap.org/about/'> | © OpenStreetMap</a>
      <a href='https://www.mapbox.com/map-feedback/' target='_blank'><strong>Improve this map</strong></a>"

    
      leaflet() %>%
        addTiles(urlTemplate = mapbox,
                 attribution = mapbox_attribution,
                 options = tileOptions(minZoom = 2, maxZoom = 7)) %>%
        #addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addLegendCustom(
          colors = factpal(levels(df$color)),
          labels = levels(df$color), sizes = c(10)
        ) %>%
        addMiniMap(
          tiles = providers$Stamen.TonerBackground, #providers$Stamen.Toner,
          toggleDisplay = TRUE
        ) %>% 
        fitBounds(-135, -55, 175, 70) %>% 
        setMaxBounds(-200, -80, 230, 95)
    
  }) # End renderLeaflet
  
  observe({

    # default if no perpetrator filter is set
    if (length(input$filterPerpetrator) == 0) {
      df.plot <- df
    } else {
      df.plot <- df[filterPerpetrators_rows(), ]
    }

    # filter by date
    df.plot <- df.plot %>%
      filter(Date >= input$filterDate[1] & Date <= input$filterDate[2])

    req(df.plot)

    # change Size depending on filter
    if (input$filterSize == 'Dead') {
      df.plot$size <- df.plot$Dead
    } else if (input$filterSize == 'Injured') {
      df.plot$size <- df.plot$Injured
    } else {
      df.plot$size <- df.plot$Injured + df.plot$Dead
    }
    
    if (nrow(df.plot) == 0) {return(NULL)}

    leafletProxy("mymap",data = df.plot) %>%
      clearMarkers() %>% 
      # addTiles(urlTemplate = mapbox,
      #          attribution = mapbox_attribution,15
      #          options = tileOptions(minZoom = 2, maxZoom = 7)) %>%
      #addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~sqrt(size) * 1.1 + 3, weight = 2,
                       color = ~factpal(color),
                       opacity = 0.2,
                       fillColor = ~factpal(color),
                       fillOpacity = 0.3,
                       #popup = ~myPopup,#~Details,
                       layerId = ~id,
                       label = ~Location)
    

  }) # End renderLeaflet
  
  # WebGL Globe - shinyglobe ------------------------------------------------

  output$myGlobe <- renderGlobe({
    
    # default if no perpetrator filter is set
    if (length(input$filterPerpetrator) == 0) {
      df.globe <- df
    } else {
      df.globe <- df[filterPerpetrators_rows(), ]
    }
    
    # filter by date
    df.globe <- df.globe %>% 
      filter(Date >= input$filterDate[1] & Date <= input$filterDate[2])
    
    req(df.globe)
    
    # change Size depending on filter
    if (input$filterSize == 'Dead') {
      df.globe$size <- df.globe$Dead
    } else if (input$filterSize == 'Injured') {
      df.globe$size <- df.globe$Injured
    } else {
      df.globe$size <- df.globe$Injured + df.globe$Dead
    }
    
    df.globe$size <- df.globe$size / max(df.globe$size)
    
    df.globe[,c("lat", "lon", "size")]
  })
  
    # timevis timeline --------------------------------------------------------

  # https://github.com/daattali/timevis
  # http://visjs.org/docs/timeline/#Configuration_Options
  
  output$myTimeline <- renderTimevis({
    
    df.timeline <- df[,c("Date", "Type", "Dead", "Injured", "Perpetrator", "id")]
    
    # default if no perpetrator filter is set
    if (length(input$filterPerpetrator) == 0) {
      df.timeline <- df.timeline
    } else {
      df.timeline <- df.timeline[filterPerpetrators_rows(), ]
    }
    
    # filter by date
    df.timeline <- df.timeline %>% 
      filter(Date >= input$filterDate[1] & Date <= input$filterDate[2])
    
    req(df.timeline)
    if (nrow(df.timeline) == 0) {return(NULL)}
    
    #df.timeline$id <- seq_len(nrow(df.timeline))
    df.timeline$start <- df.timeline$Date
    df.timeline$content <- paste(df.timeline$Injured,"/",df.timeline$Dead,"<br>",df.timeline$Type)
    df.timeline$group <- as.numeric(df.timeline$Perpetrator)
    
    df.timeline$style <- df.timeline$Perpetrator
    levels(df.timeline$style) <- paste( "background-color: ", rev(myRGBA), ";", sep = "")

    timevis(df.timeline, 
            #groups = data.frame(id = as.numeric(unique(df.timeline$Perpetrator)), content = unique(df.timeline$Perpetrator)),
            fit = FALSE, showZoom = FALSE,
            options = list(end = max(df.timeline$start) + 1,
                           start = max(df.timeline$start) - 6,
                           min = min(df.timeline$start) - 1,
                           max = max(df.timeline$start) + 1,
                           maxHeight = "500px",
                           zoomable = FALSE)
    )

  })
  
  # Infoboxes ---------------------------------------------------------------
  
  output$InjuredBox <- renderInfoBox({
    valueBox(
      incidentsStats$Injured, "Injured", icon = icon("tint", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$DeadBox <- renderInfoBox({
    valueBox(
      incidentsStats$Fatalities, "Fatalities", icon = icon("bomb", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$IncidentsBox <- renderInfoBox({
    valueBox(
      incidentsStats$Incidents, "Incidents", icon = icon("hashtag", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
})









