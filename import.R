# https://storymaps.esri.com/stories/terrorist-attacks/?year=2017

# Data Import -------------------------------------------------------------

# Data will be imported from Wikipedia. I will not use the Global Terrorism
# Database due to the fact that this data set is only updated on a yearly basis.
# https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_2017

library(rvest)
library(dplyr)
library(lubridate)

i <- "2017"
j <- "December"

import.wikipedia <- function(month, year, verbose = TRUE){
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
  
  if (verbose) {
    message(year,"-",month,": ",nrow(df)," observations ", ncol(df), " columns imported")
  }
  
  if (nrow(df) == 0) {
    warning("No observations imported.")
  }
  
  if (ncol(df) != 10) {
    warning("Number of columns deviate from 10.")
  }
  
  if (sum(is.na(df) == TRUE) > 0) {
    warning("Data contains some missing entries.") 
  }
  
  colnames(df) <- c("Date", "Type", "Dead", "Injured", "Location", "Details", "Perpetrator", "Conflict","Month","Year")
  return(df)
}

df <- import.wikipedia(j,i)
write.table(df, paste("data/raw/",i, "-", j,".csv", sep = ""), sep = ";", row.names = FALSE)

# Data Preparation --------------------------------------------------------

preprocessing <- function(df) {
  
  library(stringr)
  
  # Date
  df$Day <- sapply(str_split(df$Date,"-|\u2013|or"), function(x) as.numeric(x[1]))
  df$Date <- ymd(paste(df$Year,df$Month,df$Day))
  df$Year <- as.numeric(df$Year)
  
  # Dead
  df[df$Dead %in% c("Unknown","Several"), "Dead"] <- 0
  df$Dead <- str_replace(df$Dead, "\\+", "")
  df$Dead <- sapply(str_split(df$Dead, " |-|\\(|\u2013"), function(x) as.numeric(x[1]))
  df[is.na(df$Dead), "Dead"] <- 0
  
  # Injured
  df[df$Injured %in% c("Unknown","Several"), "Injured"] <- 0
  df$Injured <- str_replace(df$Injured, "\\+", "")
  df$Injured <- sapply(str_split(df$Injured, " |-|\\(|\u2013"), function(x) as.numeric(x[1]))
  df[is.na(df$Injured), "Injured"] <- 0
  
  # Clean text
  df$Details <- gsub("\\[.*?\\]", "", df$Details, perl = TRUE)
  
  # Perpetrator
  df$Perpetrator_orig <- df$Perpetrator
  df$Perpetrator <- tolower(df$Perpetrator)
  df$suspected <- grepl("suspected|claimed", df$Perpetrator)
  
  df$Perpetrator_cleaned <- "Unknown/Others"
  df$Perpetrator_cleaned[grepl("islamic state|abu sayyaf|wilayat sayna",df$Perpetrator)] <- "Islamic State"
  df$Perpetrator_cleaned[grepl("boko haram",df$Perpetrator)] <- "Boko Haram"
  df$Perpetrator_cleaned[grepl("taliban",df$Perpetrator)] <- "Taliban"
  df$Perpetrator_cleaned[grepl("al-shabaab|al shabaab|al-shabab",df$Perpetrator)] <- "Al-Shabaab"
  df$Perpetrator_cleaned[grepl("lone wolf",df$Perpetrator)] <- "Lone Wolf"
  df$Perpetrator_cleaned[grepl("pkk",df$Perpetrator)] <- "PKK"
  df$Perpetrator_cleaned[grepl("al-qaeda",df$Perpetrator)] <- "Al-Qaeda"
  
  df$Perpetrator <- as.factor(df$Perpetrator_cleaned)
  df$Perpetrator_cleaned <- NULL
  
  return(df)
}

df <- preprocessing(df)

# Extract lon lat Location

geocoding <- function(df) {
  
  # devtools::install_github("dkahle/ggmap") # need version 2.7 for google api
  library(ggmap)
  
  # Register Google Geocoding API
  register_google(key = GOOGLE_GEOCODING_KEY, account_type = "standard")
  #ggmap_credentials()
  
  # load already available lon/lat locations
  load("data/geocoded.RData")
  
  lookup <- df$Location[!(df$Location %in% data.geo$Location)]
  
  if (length(lookup) > 0) {
    data.geo.new <- geocode(unique(lookup))
    data.geo.new$Location <- unique(lookup)
    data.geo <- rbind(data.geo, data.geo.new)
    
    # Update geocoded db
    save(data.geo, file = "data/geocoded.RData")
  }
  
  df <- merge(df, data.geo, by = "Location")
  
  if (sum(is.na(df$lon) == TRUE) > 0) {
    warning("Geocoding not sucessful for ",sum(is.na(df$lon))," observations)")
  }
  
  return(df)
}

df <- geocoding(df)
saveRDS(df, paste("data/cleaned/",i, "-", j,".rds", sep = ""))

# Bulk import -------------------------------------------------------------

# Extract information since 2015
for (i in c("2015","2016","2017","2018")) {
  for (j in c("January","February","March","April","May","June","July","August","September","October","November","December")) {
    df <- import.wikipedia(j,i)
    write.table(df, paste("data/raw/",i, "-", j,".csv", sep = ""), sep = ";", row.names = FALSE)
    df <- preprocessing(df)
    df <- geocoding(df)
    saveRDS(df, paste("data/cleaned/",i, "-", j,".rds", sep = ""))
    Sys.sleep(sample(10, 1))
    if (i == year(Sys.Date()) & j == month(Sys.Date(), label = TRUE, abbr = FALSE)) break()
  }
  if (i == year(Sys.Date())) break()
}


