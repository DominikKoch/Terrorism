
# Todo:

# - information tab
# - Types filter
# - Options Tab (colour selection, size selection)
# - Take Tour
# - checkbox suspected
# - update icons
#   - https://www.r-bloggers.com/custom-images-for-shiny-dashboard-valuebox-icons/
#   - https://www.google.de/imgres?imgurl=https%3A%2F%2Fd30y9cdsu7xlg0.cloudfront.net%2Fpng%2F304010-200.png&imgrefurl=https%3A%2F%2Fthenounproject.com%2Fterm%2Fcounting%2F154887%2F&docid=fyPY6rzkqVKzfM&tbnid=Dj5DSxSj3IGEVM%3A&vet=10ahUKEwia1tHT_evYAhVBzKQKHegEDn0QMwiWAigRMBE..i&w=200&h=200&client=firefox-b-ab&bih=931&biw=1280&q=icon%20count&ved=0ahUKEwia1tHT_evYAhVBzKQKHegEDn0QMwiWAigRMBE&iact=mrc&uact=8
#   - https://www.google.de/search?client=firefox-b-ab&dcr=0&biw=1280&bih=931&tbm=isch&sa=1&ei=YRBmWs25EYfJsAfk16vYDQ&q=icon+patch&oq=icon+patch&gs_l=psy-ab.3..0i30k1j0i8i30k1l9.29958.30568.0.30769.5.5.0.0.0.0.85.387.5.5.0....0...1c.1.64.psy-ab..0.5.386...0j0i67k1.0.1_K44KhqmaM
#   - https://www.google.de/search?client=firefox-b-ab&dcr=0&biw=1280&bih=931&tbm=isch&sa=1&ei=JhBmWuGkO8HTsAfV0oEo&q=icon+death&oq=icon+death&gs_l=psy-ab.3..0l3j0i8i30k1l7.19078.20923.0.21164.7.6.1.0.0.0.80.458.6.6.0....0...1c.1.64.psy-ab..0.7.463....0.uYwXsn_ik_M

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

# Create own color palette
myColors <- c("Unknown/Others" = "#F2F2F2",
              "Taliban"        = "#FF0000",
              "PKK"            = "#0000FF",
              "Lone Wolf"      = "#FF00FF",
              "Islamic State"  = "#FF8000",
              "Boko Haram"     = "#FFFF00",
              "Al-Shabaab"     = "#00FF00",
              "Al-Qaeda"       = "#00FFFF")

myRGBA   <- c("Taliban"        = "rgba(255, 0, 0, 0.5)",
              "PKK"            = "rgba(0, 0, 255, 0.5)",
              "Unknown/Others" = "rgba(242,242,242, 0.5)",
              "Lone Wolf"      = "rgba(255, 0, 255, 0.5)",
              "Islamic State"  = "rgba(255, 128, 0, 0.5)",
              "Boko Haram"     = "rgba(255, 255, 0, 0.5)",
              "Al-Shabaab"     = "rgba(0, 255, 0, 0.5)",
              "Al-Qaeda"       = "rgba(0, 255, 255, 0.5)")

# myColors <- c("Unknown/Others" = "#999999",
#               "Lone Wolf"      = "#663000",
#               "Islamic State"  = "#E61A33",
#               "Boko Haram"     = "#664CFF",
#               "Taliban"        = "#FF8000",
#               "Al-Shabaab"     = "#FFFF33",
#               "PKK"            = "#33FF00",
#               "Al-Qaeda"       = "#1AB2FF")
# 
# myRGBA   <- c("Unknown/Others" = "rgba(102, 48, 0, 0.5)",
#               "Lone Wolf"      = "rgba(230, 26, 51, 0.5)",
#               "Islamic State"  = "rgba(153, 153, 153, 0.5)",
#               "Boko Haram"     = "rgba(102, 76, 255, 0.5)",
#               "Taliban"        = "rgba(255, 128, 0, 0.5)",
#               "Al-Shabaab"     = "rgba(255, 255, 51, 0.5)",
#               "PKK"            = "rgba(51, 255, 0, 0.5)",
#               "Al-Qaeda"       = "rgba(26, 178, 255, 0.5)")

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