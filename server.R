
shinyServer(function(input, output, session) {
    
  # dataset <- reactive({
  #   # Make sure requirements are met
  #   req(input$datasetName)
  #   
  #   get(input$datasetName, "package:datasets", inherits = FALSE)
  # })
  
  output$myTable <- renderTable({ 
    
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
    
    data.frame(feature = c("Incidents:", "Injured:", "Fatalities:"),
               values = prettyNum(c(df.table$KPI.incidents, df.table$KPI.injured, df.table$KPI.fatalities), big.mark = ".", decimal.mark = ","))
    
    #(c(df.table$KPI.incidents, df.table$KPI.injured, df.table$KPI.fatalities))
  },  
    spacing = 'xs',  
    colnames = FALSE,
    digits = 0
  ) 

  # Filter for Perpetrator groups
  filterPerpetrators_rows <- eventReactive(input$filterPerpetrator, {
    if (length(input$filterPerpetrator) > 0) {
      which(df$Perpetrator %in% input$filterPerpetrator)
    } else {
      return(1:nrow(df))
    }
  })
  
  # Leaflet -----------------------------------------------------------------

  df$color <- df$Perpetrator
  factpal <- colorFactor(rev(c('#999999','#663000','#E61A33','#664CFF','#FF8000','#FFFF33','#33FF00','#1AB2FF')), domain = levels(df$color), ordered = FALSE)
  
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

    # mapbox <- paste0("https://api.mapbox.com/styles/v1/datascience42/cjboxebfq6diw2rs7x1d1cpqj/tiles/256/{z}/{x}/{y}?access_token=", MAPBOX_KEY)
    # mapbox_attribution <- "<a href='http://mapbox.com/about/maps' class='mapbox-wordmark' target='_blank'>Mapbox</a>
    # <a href='https://www.mapbox.com/about/maps/'>© Mapbox</a>
    # <a href='http://www.openstreetmap.org/about/'> | © OpenStreetMap</a>
    # <a href='https://www.mapbox.com/map-feedback/' target='_blank'><strong>Improve this map</strong></a>"

      # If at least one incident occured
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
  
  # output$mymap <- renderLeaflet({
  #   
  #   # default if no perpetrator filter is set
  #   if (length(input$filterPerpetrator) == 0) {
  #     df.plot <- df
  #   } else {
  #     df.plot <- df[filterPerpetrators_rows(), ]
  #   }
  #   
  #   # filter by date
  #   df.plot <- df.plot %>% 
  #     filter(Date >= input$filterDate[1] & Date <= input$filterDate[2])
  #   
  #   req(df.plot)
  #   
  #   # change Size depending on filter
  #   if (input$filterSize == 'Dead') {
  #     df.plot$size <- df.plot$Dead
  #   } else if (input$filterSize == 'Injured') {
  #     df.plot$size <- df.plot$Injured
  #   } else {
  #     df.plot$size <- df.plot$Injured + df.plot$Dead
  #   }
  #   
  #   mapbox <- paste0("https://api.mapbox.com/styles/v1/datascience42/cjboxebfq6diw2rs7x1d1cpqj/tiles/256/{z}/{x}/{y}?access_token=", MAPBOX_KEY)
  #   mapbox_attribution <- "<a href='http://mapbox.com/about/maps' class='mapbox-wordmark' target='_blank'>Mapbox</a>
  #   <a href='https://www.mapbox.com/about/maps/'>© Mapbox</a>
  #   <a href='http://www.openstreetmap.org/about/'> | © OpenStreetMap</a>
  #   <a href='https://www.mapbox.com/map-feedback/' target='_blank'><strong>Improve this map</strong></a>"
  #   
  #   if (req(nrow(df.plot)) == 0) {
  #     # If no incident occured
  #     leaflet() %>%
  #       addTiles(urlTemplate = mapbox,
  #                attribution = mapbox_attribution,
  #                options = tileOptions(minZoom = 2, maxZoom = 7)) %>%
  #       #addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  #       addMiniMap(
  #         tiles = providers$Stamen.TonerBackground, #providers$Stamen.Toner,
  #         toggleDisplay = TRUE
  #       ) %>% 
  #       fitBounds(-135, -55, 175, 70)
  #   } else if (req(nrow(df.plot)) > 0) {
  #     # If at least one incident occured
  #     leaflet(data = df.plot) %>%
  #       addTiles(urlTemplate = mapbox,
  #                attribution = mapbox_attribution,
  #                options = tileOptions(minZoom = 2, maxZoom = 7)) %>%
  #       #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  #       addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~sqrt(size) * 1.1 + 3, weight = 2,
  #                        color = ~factpal(color),
  #                        opacity = 0.2,
  #                        fillColor = ~factpal(color),
  #                        fillOpacity = 0.3,
  #                        popup = ~myPopup,#~Details,
  #                        label = ~Location) %>% 
  #       addLegendCustom(
  #         colors = factpal(unique(df$color)), 
  #         labels = unique(df$color), sizes = c(10)
  #       ) %>% 
  #       addMiniMap(
  #         tiles = providers$Stamen.TonerBackground, #providers$Stamen.Toner,
  #         toggleDisplay = TRUE
  #       ) %>% 
  #       fitBounds(-135, -55, 175, 70)
  #   }
  #   
  # }) # End renderLeaflet
  
  # WebGL Globe - shinyglobe ------------------------------------------------
  
  
  
  # population <- readRDS("population.Rds")
  # #Filter out to only major cities -- otherwise too much data. Now ~4.5k rows
  # population <- population[population$Population > .0025, ]

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
  
  # WebGL Globe - rthreejs --------------------------------------------------

  # #data(world.cities, package="maps")
  # 
  # # earth_dark <- list(img=system.file("images/world.jpg", package="threejs"),
  # #                    bodycolor="#0011ff",
  # #                    emissive="#000010",
  # #                    lightcolor="#99ddff")
  # 
  # h <- 100 # height of the bar
  # 
  # values <- reactive({
  # 
  #   # default if no perpetrator filter is set
  #   if (length(input$filterPerpetrator) == 0) {
  #     df.globe <- df
  #   } else {
  #     df.globe <- df[filterPerpetrators_rows(), ]
  #   }
  # 
  #   # # filter by date
  #   # df.globe <- df.globe %>%
  #   #   filter(Date >= input$filterDate[1] & Date <= input$filterDate[2])
  # 
  #   req(df.globe)
  # 
  #   # change Size depending on filter
  #   if (input$filterSize == 'Dead') {
  #     df.globe$size <- df.globe$Dead
  #   } else if (input$filterSize == 'Injured') {
  #     df.globe$size <- df.globe$Injured
  #   } else {
  #     df.globe$size <- df.globe$Injured + df.globe$Dead
  #   }
  # 
  #   #cities <- df.globe#cull()
  #   value <- h * df.globe$size / max(df.globe$size)
  #   col <- rainbow(10, start = 2.8 / 6, end = 3.4 / 6)
  #   names(col) <- c()
  #   # Extend palette to data values
  #   col <- col[floor(length(col) * (h - value) / h) + 1]
  #   list(value = value, color = col, position = df.globe)
  # })
  # 
  # output$myGlobe <- renderGlobe({
  #   v <- values()
  #   args <- c(#earth_dark,
  #     list(lat = v$position$lat, long = v$position$lon, value = v$value, color = v$color, atmosphere = TRUE, bg = "#262626", rotationlat = 50.7, rotationlong = 35.5))
  #   do.call(globejs, args = args)
  # })
  
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
    
    #df.timeline <- df.timeline[1:50,]

    df.timeline$style <- df.timeline$Perpetrator
    levels(df.timeline$style) <- paste( "background-color: ", rev(c('rgba(102, 48, 0, 0.5)','rgba(230, 26, 51, 0.5)',"rgba(153, 153, 153, 0.5)",'rgba(102, 76, 255, 0.5)','rgba(255, 128, 0, 0.5)','rgba(255, 255, 51, 0.5)','rgba(51, 255, 0, 0.5)','rgba(26, 178, 255, 0.5)')), ";", sep = "")
#    levels(df.timeline$style) <- paste( "background-color: ", rev(c('#663000','#E61A33','#999999','#664CFF','#FF8000','#FFFF33','#33FF00','#1AB2FF')), ";", sep = "")
    
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
 
  # leaflet highlight -------------------------------------------------------
  
  # Hightlight the incident selected in the timeline
  
  observeEvent(input$myTimeline_selected,{

    df.highlight <- df[as.numeric(input$myTimeline_selected),]
    
    leafletProxy("mymap", data = df.highlight) %>%
      clearPopups() %>%
      addPopups(lng = ~lon, lat = ~lat, popup = ~paste(myPopup),
                options = popupOptions(closeOnClick = TRUE))
    
  })
  
  # observeEvent(input$myTakeTour, {
  #   leafletProxy("mymap") %>%
  #     clearPopups()# %>% 
  #    # clearMarkers()
  # })
  
  # Close Popups when clicking the map
  
  # observeEvent(input$mymap_click,{
  #   leafletProxy("mymap") %>%
  #     clearPopups()
  # })
  # 
  observeEvent(input$mymap_marker_click,{
    
    print(input$mymap_marker_click)
    df.highlight <- df[(input$mymap_marker_click$id),]

    leafletProxy("mymap", data = df.highlight) %>%
      clearPopups() %>% 
      addPopups(lng = ~lon, lat = ~lat, popup = ~paste(myPopup),
                options = popupOptions(closeOnClick = TRUE))
  })
  
})






