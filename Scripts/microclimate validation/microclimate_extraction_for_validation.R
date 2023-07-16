library(NicheMapR)
library(tidyverse)
library(raster)
library(doParallel)
library(foreach)

year_id <- as.character(1990:2022)
sites <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)



# Raw weather data ERA5 ---------------------------------------------------

era5_df <- data.frame()


for(year in 1:length(year_id)){

  # Extract raw ERA5 data

  era5 <- stack(paste0("Spatial_Data/ERA5_",year_id[year],".nc"))

  for(site in 1:nrow(sites)){


    xy <- data.frame(x = sites[site, 3], y = sites[site, 2])
    sp::coordinates(xy) = ~x + y
    sp::proj4string(xy) = "+init=epsg:4326"

    ext <- as.data.frame(raster::extract(era5, xy))
    ext <- ext %>% gather("date", "temp")
    ext <- ext %>% mutate(temp = (temp - 273.15)) # transform from kelvin to celsius
    ext$site <- sites[site,1]

    # Function to reformat the date column
    reformat_date <- function(date_column) {
      # Extract date and time components using regular expressions
      date_only <- str_extract(date_column, "\\d{4}\\.\\d{2}\\.\\d{2}")
      date_time <- str_extract(date_column, "\\d{4}\\.\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{2}")

      # Replace "X" prefix with empty string
      date_only <- str_replace(date_only, "X", "")
      date_time <- str_replace(date_time, "X", "")

      # Handle missing time at midnight
      date_time <- ifelse(is.na(date_time), str_c(date_only, ".00.00.00"), date_time)

      # Combine date and time components
      result <- ifelse(!is.na(date_time), date_time, date_only)

      # Convert to POSIXct format
      as.POSIXct(result, format = "%Y.%m.%d.%H.%M.%S")
    }

    # Apply the function to your date column
    ext$date <- reformat_date(ext$date)
    ext$date <- ext$date + hours(10)
    ext$year <- lubridate::year(ext$date)
    ext$month <- lubridate::month(ext$date)
    ext$day <- lubridate::day(ext$date)
    ext <- ext %>% group_by(site, year, month, day) %>% summarise(tmin = min(temp, na.rm = T), tmax = max(temp, na.rm = T))
    
    era5_df <- rbind(era5_df, ext)

  }
}

# Check dataset and spot errors
unique(era5_df$year)
unique(era5_df$site)

# Convert dates to Date class
date1 <- as.Date("1990-01-01")
date2 <- as.Date("2022-12-31")

# Calculate the number of days
num_days <- as.numeric(date2 - date1)

num_days * 32 


nrow(era5_df) - num_days * 32

# there are 1056 extra days, this is because after the conversion, I have for most year repeated first of january

367*32*length(unique(met_df$year))
mean(c(367*32*length(unique(met_df$year)), 366*32*length(unique(met_df$year))))

era5_df <- era5_df %>% filter(year %in% c(1990:2022))

write.csv(era5_df, "microclimate/raw_weather_data_era5.csv")

# Microclimate data (ERA5) generated with NicheMapR -----------------------

met_df <- data.frame()

for (i in 1:nrow(sites)) {
  load(paste0('model_output/microclimate_possum/micro_', sites[i,1], '_1990_2022.Rda')) # for possum sites
  #load(paste0('model_output/', microclimate,'/micro_', sites[i], '.Rda')) # for leahy sites

  # Adjust date ( this won't be needed for future models, as the date is already)
  dates <- micro$dates
  gmtzone <- ""
  tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
  attr(dates, "tzone") <- tz

  # Met
  met <- as.data.frame(micro$shadmet)
  met$site <- sites[i,1]
  met$date <- dates
  met$year <- lubridate::year(met$date)
  met$month <- lubridate::month(met$date)
  met$day <- lubridate::day(met$date)
  met <- met %>% group_by(site, year, month, day) %>% summarise(tmin = min(TAREF, na.rm = T), tmax = max(TAREF, na.rm = T),
                                                                tmin_sky = min(TSKYC, na.rm = T), tmax_sky = max(TSKYC, na.rm = T))
  met_df <- rbind(met_df, met)
}

unique(met_df$year)
unique(met_df$site)
met_df <- met_df %>% filter(!year == 2023)

365*32*length(unique(met_df$year))

write.csv(met_df, "microclimate/microclimate_generation_era5.csv")

# Lily's data -------------------------------------------------------------

lily <- read.csv("Data/climate/Leahy_Atherton_Carbine_HOBO_2019-21.csv")

lily <- lily %>% filter(Vertical == "CANOPY")

lily$date <- strptime(lily$Date.Time, "%d/%m/%Y %H:%M")
lily$year <- year(lily$date)
lily$month <- month(lily$date)
lily$day <- day(lily$date)
lily <- lily %>% mutate(site = case_when(
  Site == "AU10" ~ "AU10A",
  Site == "AU6" ~ "AU6A",
  Site == "CU12" ~ "CU12A",
  Site == "CU6" ~ "CU6A",
  TRUE ~ Site
))
lily <- lily %>% group_by(site, year, month, day) %>% summarise(tmin = min(Temperature, na.rm = T),
                                                                tmax = max(Temperature, na.rm = T))

write.csv(lily, "microclimate/lily.csv")

# Extract Storlei daily data ----------------------------------------------
# Load libraries

library(raster)
library(dplyr)
library(foreach)
library(doParallel)

sites <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)

# Set up paths and folders
main_folder <- "~/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - Spatial data/awt_daily_temperature"
tmax_folder <- file.path(main_folder, "TMAX")
tmin_folder <- file.path(main_folder, "TMIN")

# Function to extract temperature values for a given date and site
extract_temperature <- function(date, latitude, longitude) {
  year_folder <- format(date, "%Y")
  tmax_file <- file.path(tmax_folder, year_folder, paste0("TMAX.", format(date, "%Y%m%d"), ".tif"))
  tmin_file <- file.path(tmin_folder, year_folder, paste0("TMIN.", format(date, "%Y%m%d"), ".tif"))
  
  # Check if the files exist before attempting to read them
  if (!file.exists(tmax_file)) {
    print(paste("TMAX file not found:", tmax_file))
    return(NULL)  # Skip this date
  }
  if (!file.exists(tmin_file)) {
    print(paste("TMIN file not found:", tmin_file))
    return(NULL)  # Skip this date
  }
  
  
  tmax_raster <- raster::raster(tmax_file)
  tmin_raster <- raster::raster(tmin_file)
  
  tmax_value <- raster::extract(tmax_raster, cbind(sites$long, sites$lat))
  tmin_value <- raster::extract(tmin_raster, cbind(sites$long, sites$lat))
  
  return(data.frame(site = sites$site,
                    latitude = sites$lat,
                    longitude = sites$lat,
                    date = format(date, "%Y-%m-%d"),
                    year = as.integer(format(date, "%Y")),
                    month = as.integer(format(date, "%m")),
                    day = as.integer(format(date, "%d")),
                    tmax = tmax_value,
                    tmin = tmin_value))
}

# Set up data to loop through
years <- 1990:2016 # test
sites_data <- sites %>%
  dplyr::select(lat, long) # We only need latitude and longitude for the extraction


# Register all available cores for parallel processing
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


# Parallel loop to extract temperature data
result_df <- foreach(date = seq(as.Date("1990-01-01"), as.Date("2016-12-31"), by = "day"),
                     .combine = rbind) %dopar% {
                       extract_temperature(date, sites_data$lat, sites_data$long)
                     }

# Stop the parallel backend
stopCluster(cl)

# Save results

write.csv(result_df, "microclimate/storlei_daily.csv")
