library(tidyverse)
library(NicheMapR)
library(zoo)
library(data.table)
library(ISOweek)

# Cheatsheet
species <- c("archeri", "cinereus", "lemuroides", "herbertensis")
prefixes <- c("enbal", "masbal", "morph", "treg")
variable_names <- c("TC", "H2O", "energy", "PANT")
#sites_sel <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)
sites_sel <- c("AU2", "AU6", "AU10", "CU1", "CU6", "CU12")

# Function
gen_var <- function(species_sel = "archeri",
                    sites = sites_sel, # assuming that your output files are called (for example): enbal_AU2_archeri.csv (model_site_species.csv)
                    model = "enbal",
                    variable_name = "energy", # for water just use H2O, the function already does the addition of both; for Qgen just use the variable name "energy"
                    period = c(24*7), # in hours for the rolling mean
                    threshold = 36.8, # threshold to define stress, here is for TC specifically
                    hour_threshold = 5, # consecutive hours stress
                    ystart = 2019,
                    yfinish = 2021,
                    output = "output_model_1_leahy",
                    microclimate = "microclimate_leahy") {
  
  ### Start of the function
  
  # read files for the specific model out (prefix) and the species of interest
# Read files for the specific model output (prefix) and the species of interest
file_names <- list.files(path = paste0("model_output/", output, "/"), pattern = paste0("^", model, ".*", species_sel,"_", "[0-9]+\\.csv$"))
numbers <- sub(paste0("^.*", species_sel, ".*_([0-9]+)\\.csv$"), "\\1", file_names)
datasets <- list()

# Read all sites for the combination of model output and species
for (i in 1:length(file_names)) {
  dataset <- read.csv(file.path("model_output", output, file_names[i]))
  dataset$shade <- as.numeric(numbers[i])
  datasets[[i]] <- dataset
}

  
  
  # Retrieve microclimate variables
  met_df <- data.frame()
  soil_df <- data.frame()
  rain_df <- data.frame()
  
  for (i in 1:length(sites)) {
    load(paste0('model_output/', microclimate,'/micro_', sites[i], '.Rda')) # for possum sites
    #load(paste0('model_output/', microclimate,'/micro_', sites[i], '.Rda')) # for leahy sites
    
    # Adjust date ( this won't be needed for future models, as the date is already)
    dates <- micro$dates
    gmtzone <- ""
    tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
    attr(dates, "tzone") <- tz
    # note that the transformation is not needed here for some reason
    # the reason is that when saved to csv, the timezone transformation is lost, that is why the output from the previous model is wrong (the date i mean)
    
    # Met
    met <- as.data.frame(micro$shadmet)
    met$site <- sites[i]
    met$date <- dates
    met_df <- rbind(met_df, met)
    # Soil
    soil <- as.data.frame(micro$shadsoil)
    soil$site <- sites[i]
    soil$date <- dates
    soil_df <- rbind(soil_df, soil)
    # Rain
    rain <- tibble(rain = micro$RAINFALL)
    dstart <- as.Date(paste0("01/01/", ystart), format = "%d/%m/%Y")
    dfinish <- as.Date(paste0("31/12/", yfinish), format = "%d/%m/%Y")
    #dates <- seq(dstart, dfinish, by = "day")
    rain$date <- (micro$dates2)
    rain$doy <- yday(rain$date)
    rain$year <- year(rain$date)
    rain$site <- sites[i]
    rain$rain_bin <- ifelse(rain$rain>0,1,0)
    rain_df <- rbind(rain_df, rain)
  }
  
  # Compile data selected
  data <- do.call(rbind, datasets)
  data$dates <- met$date # because I have all sites in a rbind file, so I need this date
  data$year <- as.integer(strftime(data$dates, format = "%Y"))
  data$week <- as.integer(strftime(data$dates, format = "%V"))
  
  # Conditional variation of certain parameters
  
  if(variable_name == "H2O"){
    data$H2O <- (data$H2OResp_g + data$H2OCut_g)
  }
  
  if(variable_name == "energy"){
    
    data$energy <- data$QGEN * 3.6 # hourly energy requirement in KJ
  }

  
  return(list(
   
    microclimate_df = met_df,
    soil_df = soil_df,
    rain_df = rain_df,
    full_data = data,
    species_sel = species_sel,
    sites = sites,
    model = model,
    variable_name = variable_name,
    period = period,
    threshold = threshold,
    hour_threshold = hour_threshold,
    ystart = ystart,
    yfinish = yfinish,
    output = output
  ))
  
}


# test some stuff 

out <- gen_var()

out$microclimate_df$hour <- hour(out$microclimate_df$date)

out$microclimate_df[1:24,] %>% ggplot(aes(hour, SOLR))+
  geom_point()


dat <- out$full_data

dat %>%  filter(dates >= as.POSIXct("2021-08-01 08:00:00") & 
                  dates <= as.POSIXct("2021-08-03 08:00:00") )%>% 
  ggplot(aes(dates, QGEN, col = shade))+
  geom_point()+
  scale_x_datetime(guide = guide_axis(n.dodge=2))+ 
  facet_wrap(~site)+
  theme_classic()


# Climate change - temperature - plot -------------------------------------
# 
# met <- out$microclimate_df
# 
# met$year <- year(met$date)
# 
# met$month <- month(met$date)
# 
# met2 <- met %>% group_by(site, year, month) %>% 
#   summarise(TAREF = mean(TAREF, na.rm = T)) %>% 
#   arrange(site, year, month) %>% ungroup() %>% 
#   group_by(site) %>% mutate(roll = rollmeanr(TAREF, k = 36, fill = NA)) # 3 years window average
# 
# mat <- met %>% group_by(year, site) %>% summarise(mat = mean(TAREF, na.rm = T))
# 
# 
# met2 %>% 
#   ggplot(aes(year, roll))+
#   geom_point(shape = 21, stroke = 1)+
#   geom_line(data = mat, aes(year, mat), col = "red")+
#   geom_smooth()+
#   facet_wrap(~site, scales = "free")+
#   theme_classic()
