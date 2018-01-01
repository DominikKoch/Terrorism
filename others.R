# https://storymaps.esri.com/stories/terrorist-attacks/?year=2017

# Data Import -------------------------------------------------------------

# Data will be imported from Wikipedia. I will not use the Global Terrorism
# Database due to the fact that this data set is only updated on a yearly basis.
# https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_2017

library(rvest)
library(dplyr)
library(lubridate)

import.wikipedia <- function(month, year){
  url <- paste("https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in",month,year,sep = "_")

  tmp <- url %>% 
      read_html %>%
      html_nodes("table")
  
  # # Prevent bug due to cleanup info box
  # if (length(tmp) == 4) {
  #   df <- (as.data.frame(html_table(tmp[2], fill = TRUE)))
  # } else {
  #   df <- (as.data.frame(html_table(tmp[1], fill = TRUE)))
  # }
  
  # find the terrorIncidents table
  df <- (as.data.frame(html_table(tmp[grep("terrorInc",html_attrs(tmp))], fill = TRUE)))

  df$month <- month
  df$year <- year
  
  colnames(df) <- c("Date", "Type", "Dead", "Injured", "Location", "Details", "Perpetrator", "Conflict","Month","Year")
  return(df)
}

# df <- import.wikipedia("August","2016")

incidents <- list()

# Extract information since 2015
for (i in c("2015","2016","2017","2018")) {
  for (j in c("January","February","March","April","May","June","July","August","September","October","November","December")) {
    cat(i,j,"\n")
    incidents[[paste(i, j, sep = "-")]] <- import.wikipedia(j,i)
    if (i == year(Sys.Date()) & j == month(Sys.Date(), label = TRUE, abbr = FALSE)) break()
  }
  if (i == year(Sys.Date())) break()
}

# Combine lists into one big data.frame
df <- do.call(rbind.data.frame, incidents)

#sapply(incidents, dim)

# Export raw data
write.table(df, file = "data/data_raw.csv", sep = ",", row.names = FALSE)

# Data Preparation --------------------------------------------------------

# Type
library(stringr)
#library(splitstackshape)


#df$new <- str_split(df$Type, pattern = ",")
# TODO: multiple types analysieren
# TODO: vereinheitlichen (z.B. Execution = Executions, Car Bomb = Car Bombing)
#tmp <- cSplit_e(df, "Type", ",", type = "character", fill = 0)

# Date
df$Day <- sapply(str_split(df$Date,"-|\u2013|or"), function(x) as.numeric(x[1]))
df$Date <- ymd(paste(df$Year,df$Month,df$Day))

# Dead
df[df$Dead == "Unknown", "Dead"] <- 0
df$Dead <- str_replace(df$Dead, "\\+", "")
#df$Dead <- sapply(str_split(df$Dead, " "), function(x) as.numeric(x[1]))
df$Dead <- sapply(str_split(df$Dead, " |-|\\(|\u2013"), function(x) as.numeric(x[1]))
df[is.na(df$Dead), "Dead"] <- 0

# Injured
df[df$Injured == "Unknown", "Injured"] <- 0
df$Injured <- str_replace(df$Injured, "\\+", "")
#df$Injured <- sapply(str_split(df$Injured, " "), function(x) as.numeric(x[1]))
df$Injured <- sapply(str_split(df$Injured, " |-|\\(|\u2013"), function(x) as.numeric(x[1]))
df[is.na(df$Injured), "Injured"] <- 0

# Export cleaned data
write.table(df, file = "data/data_cleaned.csv", sep = ",", row.names = FALSE)

# Extract lon lat Location
library(ggmap)

data.geo <- geocode(unique(df$Location))
data.geo$Location <- unique(df$Location)

save(data.geo, file = "data/geocoded.RData")


# data.geo <- geocode(df$Location)
# df.geo <- data.geo
# df$lon <- df.geo$lon
# df$long <- df.geo$lon
# df$lat <- df.geo$lat


# Load Data ---------------------------------------------------------------

df <- read.csv("data/data_cleaned.csv")
load("data/geocoded.RData")

non.match <- merge(df, data.geo, by = "Location", all.x = TRUE)
non.match <- non.match %>% filter(is.na(lon))
rm(non.match)

# Merge Data
df <- merge(df, data.geo, by = "Location")

# drop empty obs
df <- df %>% filter(is.na(lon) == FALSE)

# Clean text
df$Details <- gsub("\\[.*?\\]", "", df$Details, perl = TRUE)

df$Dead <- as.numeric(as.character(df$Dead))
df$Injured <- as.numeric(as.character(df$Injured))

df$Date <- ymd(df$Date)
df$Dead <- as.numeric(as.character(df$Dead))
df$Injured <- as.numeric(as.character(df$Injured))

df$Year <- as.numeric(as.character(df$Year))
df$Month <- as.character(df$Month)

df <- df %>% filter(is.na(Year) == FALSE)


df$test <- droplevels(df$Perpetrator)
df$test <- tolower(df$test)
df$suspected <- grepl("suspected|claimed", df$test)

df$Perpetrator_cleaned <- "Unknown/Others"
df$Perpetrator_cleaned[grepl("islamic state|abu sayyaf|wilayat sayna",df$test)] <- "Islamic State"
df$Perpetrator_cleaned[grepl("boko haram",df$test)] <- "Boko Haram"
df$Perpetrator_cleaned[grepl("taliban",df$test)] <- "Taliban"
df$Perpetrator_cleaned[grepl("al-shabaab|al shabaab|al-shabab",df$test)] <- "Al-Shabaab"
df$Perpetrator_cleaned[grepl("lone wolf",df$test)] <- "Lone Wolf"
df$Perpetrator_cleaned[grepl("pkk",df$test)] <- "PKK"
df$Perpetrator_cleaned[grepl("al-qaeda",df$test)] <- "Al-Qaeda"

df$Perpetrator <- as.factor(df$Perpetrator_cleaned)

# Create own color palette
myColors <- c("Unknown/Others" = "#999999",
              "Lone Wolf" = "#663000",
              "Islamic State" = "#E61A33",
              "Boko Haram" = "#664CFF",
              "Taliban" = "#FF8000",
              "Al-Shabaab" = "#FFFF33",
              "PKK" = "#33FF00",
              "Al-Qaeda" = "#1AB2FF")

# myColorsPale <- c("Unknown/Others" = "#E6E6E6",
#               "Lone Wolf" = "#CC9B7A",
#               "Islamic State" = "#FF99BF",
#               "Boko Haram" = "#CCBFFF",
#               "Taliban" = "#FFBF80",
#               "Al-Shabaab" = "#FFFF99",
#               "PKK" = "#B2FF8C",
#               "Al-Qaeda" = "#A6EDFF")



# check <- df %>% filter(Perpetrator_cleaned == "PKK")
# check <- df %>% filter(Perpetrator_cleaned == "Unknown/Others")

# Plot --------------------------------------------------------------------

map.world <- map_data("world")

# get rid of Antarctica
map.world <- map.world[map.world$region != "Antarctica",]

library(ggplot2)
library(viridis) # scale_fill_viridis
library(RColorBrewer)

# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#reproducibility
theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Courier New", color = "#2B2B2B"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# https://stackoverflow.com/questions/32649426/ggplot-combining-size-and-color-in-legend
# color: http://sharpsightlabs.com/blog/human-capital-map/
# combine legend through identical name + limits

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#2B2B2B", linetype = "blank") +
  geom_point(data = df, aes(x = lon, y = lat, size = Dead, color = Perpetrator), alpha = .5) +
  geom_point(data = df, aes(x = lon, y = lat, size = Dead, color = Perpetrator), shape = 1) +
  #scale_color_viridis(name = "test", option = "plasma") +
  scale_colour_manual(values = myColors) +
  #scale_color_gradientn(colours = rev(heat.colors(8))) +
  #scale_color_gradient2(name = "Fatalities", limits = c(0,500), breaks = c(0,50,100,500), low = "green", mid = "yellow", high = "red", midpoint = 50) +
  scale_size_continuous(name = "Fatalities", limits = c(0,500), range = c(.5, 5), breaks = c(0,50,100,500)) +
  #guides(color = guide_legend(reverse = TRUE, override.aes = list(alpha = 1, size = 5) )) +
  guides(color = guide_legend(override.aes = list(size = 3),
                              title.position = 'top',
                              title.hjust = 0.0
                              ),
         size = guide_legend(title.position = 'top',
                             title.hjust = 0.1)) +
  #coord_map(xlim = c(-180,180)) +
  coord_map("rectangular", lat0 = 0, ylim = c(-55, 90), xlim = c(-180,180)) +
  labs(title = "Terrorist incidents between 2015 and 2017"
       ,subtitle = "Terrorism related to drug wars and cartel violence is not included"
       ,caption = "Map CC-BY-SA; Author: Dominik Koch (dominikkoch.github.io)\nData: https://en.wikipedia.org/w/index.php?title=List_of_terrorist_incidents") +
  theme_map() +
  theme(text = element_text(colour = "#444444", family = "Gill Sans")
        # # Change background colour
        # ,panel.background = element_rect(fill = alpha('white', 0.0))
        # ,plot.background = element_rect(fill = alpha('white', 0.0))
        # ,legend.background = element_rect(fill = alpha('white', 0.0))
        # # Elimnate grid
        # ,panel.grid = element_blank()
        # # Eliminate axis
        # ,axis.title = element_blank()
        # ,axis.ticks = element_blank()
        # ,axis.text = element_blank()
        # Margin
        # ,plot.margin = unit(c(.5,.5,.2,.5), "cm")
        # ,panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm")
        #,panel.border = element_blank()
        # Adjust titles
        ,plot.title = element_text(hjust = 0.5, color = "#2B2B2B")
        ,plot.subtitle = element_text(hjust = 0.5,  color = "#2B2B2B")
        ,plot.caption = element_text(size = 6, 
                                    hjust = 1, 
                                    # margin = margin(t = 0.2, 
                                    #                b = 0, 
                                    #                unit = "cm"), 
                                    color = "#939184")
        # Adjust legend
        #,legend.key = element_rect(fill = alpha('white', 0.0))
        ,legend.text = element_text(size = 7, hjust = 0, color = "#2B2B2B")
        ,legend.position = "bottom" #c(.1,.175)
        ,legend.title = element_text(size = 8)
  ) 

# todo
# colour points
# try other data for maps # https://github.com/tidyverse/ggplot2/issues/1104

# colors from dichromat
# colorschemes$Categorical.12
# [1] "#FFBF80" "#FF8000" "#FFFF99" "#FFFF33" "#B2FF8C" "#33FF00" "#A6EDFF" "#1AB2FF"
# [9] "#CCBFFF" "#664CFF" "#FF99BF" "#E61A33"

# Dark themed
# ggplot() +
# geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#191A1A", linetype = "blank") +
#   geom_point(data = df, aes(x = lon, y = lat, size = Dead, color = Dead), alpha = .5) +
#   geom_point(data = df, aes(x = lon, y = lat, size = Dead, color = Dead), shape = 1) +
#   scale_color_gradient2(name = "Fatalities", limits = c(0,500), breaks = c(0,50,100,500), low = "green", mid = "yellow", high = "red", midpoint = 50) +
#   scale_size_continuous(name = "Fatalities", limits = c(0,500), range = c(.9, 11), breaks = c(0,50,100,500)) +
#   #guides(color = guide_legend(reverse = TRUE, override.aes = list(alpha = 1, size = 5) )) +
#   guides(color = guide_legend(), size = guide_legend()) +
#   labs(title = "Possible cities for new Amazon Headquarters"
#        ,subtitle = "Based on population & percent of people with college degrees"
#        ,caption= "data source") +
#   theme(text = element_text(colour = "#444444", family = "Gill Sans")
#         # Change background colour
#         ,panel.background = element_rect(fill = "#343332")
#         ,plot.background = element_rect(fill = "#343332")
#         ,legend.background = element_rect(fill = "#343332")
#         # Elimnate grid
#         ,panel.grid = element_blank()
#         # Eliminate axis
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,axis.text = element_blank()
#         # Adjust titles
#         ,plot.title = element_text(size = 28)
#         ,plot.subtitle = element_text(size = 12)
#         # Adjust legend
#         ,legend.key = element_rect(fill = "#343332", colour = "#343332")
#         ,legend.position = "bottom"
#   ) 
  

library(leaflet)
data(quakes)

# # Plot markers on map
# leaflet(data = quakes[1:20,]) %>% 
#   addTiles() %>% 
#   #addMarkers(clusterOptions = markerClusterOptions())
#   addMarkers(~long, ~lat, popup = ~as.character(depth), label = ~as.character(mag))

# # Automatic clusters on map
# leaflet(data = df) %>% 
#   addTiles() %>% 
#   addMarkers(~lon, ~lat, clusterOptions = markerClusterOptions())

# # Dummy Colours
# df$color <- as.factor(sample(1:5, size = nrow(df), replace = TRUE))
# factpal <- colorFactor(topo.colors(5), domain = df$color)



# leaflet(data = df) %>% 
#   addTiles() %>% 
#   addProviderTiles(providers$CartoDB.DarkMatter) %>% 
#   addCircles(lng = ~lon, lat = ~lat, radius = ~Dead*10000, weight = 5,
#              color = ~factpal(color),
#              fillColor = ~factpal(color),
#              stroke = TRUE,
#              opacity = 0.8,
#              fillOpacity = 0.4)

df$color <- as.numeric(as.character(df$Year))
factpal <- colorFactor(topo.colors(3), domain = df$color)

df$color <- df$Perpetrator
factpal <- colorFactor(rev(c('#999999','#663000','#E61A33','#664CFF','#FF8000','#FFFF33','#33FF00','#1AB2FF')), domain = df$color)

leaflet(data = df) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~sqrt(Dead) * 1.0 + 3, weight = 2,
                   color = ~factpal(color),
                   opacity = 0.3,
                   fillColor = ~factpal(color),
                   fillOpacity = 0.2,
                   popup = ~Details,
                   label = ~Location)


# https://api.mapbox.com/styles/v1/mapbox/dark-v9.html?title=true&access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NDg1bDA1cjYzM280NHJ5NzlvNDMifQ.d6e-nNyBDtmQCVwVNivz7A#2/0/0


# https://rpubs.com/walkerke/rstudio-mapbox
ugly_map <- "https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjaW9jenN1OGwwNGZsdjRrcWZnazh2OXVxIn0.QJrmnV9lJzdXHkH95ERdjw"

mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"

leaflet(df) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~sqrt(Dead) * 1.5 + 3, weight = 2,
                   color = ~factpal(color),
                   opacity = 0.3,
                   fillColor = ~factpal(color),
                   fillOpacity = 0.2,
                   popup = ~Details,
                   label = ~Location) %>% 
  addTiles(urlTemplate = ugly_map, attribution = mb_attribution)


# Hosting tokens
# https://github.com/STAT545-UBC/Discussion/issues/301
# https://www.mapbox.com
# https://api.mapbox.com/styles/v1/mapbox/dark-v9.html?title=true&access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NDg1bDA1cjYzM280NHJ5NzlvNDMifQ.d6e-nNyBDtmQCVwVNivz7A#0.86/0/0