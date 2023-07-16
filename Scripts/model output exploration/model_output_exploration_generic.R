library(tidyverse)
library(lubridate)


# Load and combine all data - do this only once (heavy computation) -------

# library(data.table)
# 
# folder_path <- "model_output/output_model_1_possum/archeri"
# 
# categories <- c("enbal", "masbal", "treg", "micro", "morph")
# 
# file_list <- list.files(folder_path, full.names = TRUE)
# 
# file_groups <- lapply(categories, function(category) {
#   group_files <- file_list[startsWith(basename(file_list), category)]
#   if (length(group_files) > 0) {
#     return(group_files)
#   } else {
#     return(NULL)
#   }
# })
# 
# 
# combined_data_list <- lapply(file_groups, function(group) {
#   data_list <- lapply(group, function(file_path) {
#     fread(file_path, header = TRUE)  # Using fread from data.table to read the files
#   })
#   rbindlist(data_list)
# })
# combined_data_by_category <- setNames(combined_data_list, categories)
# 
# 
# 
# enbal <- combined_data_by_category$enbal
# masbal <- combined_data_by_category$masbal
# treg <- combined_data_by_category$treg
# micro <- combined_data_by_category$micro
# morph <- combined_data_by_category$morph
# 
# write.csv(enbal, "model_output/output_model_1_possum/archeri/enbal_bind.csv")
# write.csv(masbal, "model_output/output_model_1_possum/archeri/masbal_bind.csv")
# write.csv(treg, "model_output/output_model_1_possum/archeri/treg_bind.csv")
# write.csv(micro, "model_output/output_model_1_possum/archeri/micro_bind.csv")
# write.csv(morph, "model_output/output_model_1_possum/archeri/morph_bind.csv")



# Retrieve rain from microclimate -----------------------------------------

sites <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)
ystart = 1990
yfinish = 2022

rain_df <- data.frame()

for (i in 1:nrow(sites)) {
  load(paste0('model_output/microclimate_possum/micro_', sites[i,1], '_1990_2022.Rda')) # for possum sites
  dates <- micro$dates
  gmtzone <- ""
  tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
  attr(dates, "tzone") <- tz
  
  rain <- tibble(rain = micro$RAINFALL)
  dstart <- as.Date(paste0("01/01/", ystart), format = "%d/%m/%Y")
  dfinish <- as.Date(paste0("31/12/", yfinish), format = "%d/%m/%Y")
  #dates <- seq(dstart, dfinish, by = "day")
  rain$date <- (micro$dates2)
  rain$doy <- yday(rain$date)
  rain$year <- year(rain$date)
  rain$site <- sites[i,1]
  rain$rain_bin <- ifelse(rain$rain>0,1,0)
  rain_df <- rbind(rain_df, rain)
}

unique(rain_df$year)
unique(rain_df$site)

# Load combined files -----------------------------------------------------

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


enbal <- read.csv("model_output/output_model_1_possum/archeri/enbal_bind.csv")
enbal$dates <- ifelse(nchar(enbal$dates) == 10, paste0(enbal$dates, " 00:00:00"), enbal$dates)
enbal$dates <- as.POSIXct(enbal$dates, format = "%Y-%m-%d %H:%M:%S")
enbal$dates <- enbal$dates + hours(10)

masbal <- read.csv("model_output/output_model_1_possum/archeri/masbal_bind.csv")
masbal$dates <- ifelse(nchar(masbal$dates) == 10, paste0(masbal$dates, " 00:00:00"), masbal$dates)
masbal$dates <- as.POSIXct(masbal$dates, format = "%Y-%m-%d %H:%M:%S")
masbal$dates <- masbal$dates + hours(10)

treg <- read.csv("model_output/output_model_1_possum/archeri/treg_bind.csv")
treg$dates <- ifelse(nchar(treg$dates) == 10, paste0(treg$dates, " 00:00:00"), treg$dates)
treg$dates <- as.POSIXct(treg$dates, format = "%Y-%m-%d %H:%M:%S")
treg$dates <- treg$dates + hours(10)

microclim <- read.csv("model_output/output_model_1_possum/archeri/micro_bind.csv")
microclim$dates <- ifelse(nchar(microclim$dates) == 10, paste0(microclim$dates, " 00:00:00"), microclim$dates)
microclim$dates <- as.POSIXct(microclim$dates, format = "%Y-%m-%d %H:%M:%S")
microclim$dates <- microclim$dates + hours(10)

morph <- read.csv("model_output/output_model_1_possum/archeri/morph_bind.csv")
morph$dates <- ifelse(nchar(morph$dates) == 10, paste0(morph$dates, " 00:00:00"), morph$dates)
morph$dates <- as.POSIXct(morph$dates, format = "%Y-%m-%d %H:%M:%S")
morph$dates <- morph$dates + hours(10)

rain <- rain_df



head(enbal)
head(masbal)
head(treg)
head(microclim)
head(morph)
