
# Todo:


# - information tab
# - Infoboxes

# - Types filter
# - Options Tab (colour selection, size selection)

# Done

# - ciruclar legend
# - leaflet minimap
# - Infoboxes
# - validate & need
# - UI layout
# - Popups
# - Weltkugel
# - https://www.mapbox.com
# - mapbox attribution (https://www.mapbox.com/help/how-attribution-works/#mapbox-wordmark)
# - timevis
#   - http://visjs.org/docs/timeline/#Configuration_Options
#   - https://github.com/daattali/timevis
# - timevis/leaflet selection highlight
#   - leafletproxy (https://rstudio.github.io/leaflet/shiny.html)
# - shiny globe zoom crash (switched back to shinyglobe)
# - shinyapps.io
# - Datenaufbereitung auslagern
# - fixed colors
# - fixed legend order
# - only one active popup
# - added leaflet maxBounds
# - leaflet double legend
# - leaflet filter Perpetrator update
# - github

# Load packages -----------------------------------------------------------

library(shiny) # R Shiny interface
library(shinydashboard) # R Shiny framework
library(shinyBS)

library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(lubridate) # date manipulation

library(maps)    # country borders
library(leaflet) # interactive maps
library(ggplot2) # plots
library(viridis) # colors scale_fill_viridis
library(RColorBrewer) # colors
library(shinyGlobe) # webGL Globe # killed my css when using globeOutput()
#library(threejs) # webGL Globe # zoom crashed the visualization
library(timevis) # timeline

#devtools::install_github("bwlewis/rthreejs")

# Load data ---------------------------------------------------------------

# load all .rds files in the data/cleaned path

files = list.files(path = 'data/cleaned/', pattern = '\\.rds$')
df.list = lapply(files, function(x) readRDS(paste("data/cleaned/",x, sep = "")))
df <- do.call(rbind.data.frame, df.list)
rm(df.list, files)

# drop missing locations
df <- df %>% filter(is.na(lon) == FALSE)

df$id <- seq_len(nrow(df))

df$myPopup <- paste0("<strong>Date: </strong>", 
                      df$Date, 
                     "<br><strong>Perpetrator: </strong>", 
                      df$Perpetrator_orig, 
                     "<br><strong>Type: </strong>", 
                      df$Type, 
                     "<br><strong>Injuries/Fatalities: </strong>", 
                      df$Injured, "/", df$Dead,
                     "<hr style='margin:4px'/>",
                      df$Details)

# Styling -----------------------------------------------------------------

# Actionbutton
action_btn_style <- "color: black; margin: 10px 15px 10px 15px; width: 200px;"

# Custom functions --------------------------------------------------------

# Add circular legend to leaflet

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

# Overwrite globeOutput

.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyGlobe',
      directoryPath = system.file('www', package = 'shinyGlobe'))
    .global$loaded <- TRUE
  }
  HTML("")
}

globeOutput <- function(outputId){
  tagList(
    singleton(tags$head(
      initResourcePaths(),
      tags$script(src = 'shinyGlobe/third-party/three.min.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/third-party/Detector.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/third-party/Tween.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/globe.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/shinyGlobe.js', type = "text/javascript")#,
      # includeCSS(system.file("/www/style.css", package = "shinyGlobe"))
    )),
    div(id = outputId, class = "shiny-globe-output") 
  )
}