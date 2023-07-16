library(tidyverse)
library(NicheMapR)
library(zoo)
library(data.table)
library(ISOweek)

# Cheatsheet
species <- c("archeri", "cinereus", "lemuroides", "herbertensis")
prefixes <- c("enbal", "masbal", "morph", "treg")
variable_names <- c("TC", "H2O", "energy", "PANT")
sites_sel <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)
sites_sel <- unique(sites_sel$site)

micro_adjust <- read.csv("Data/data_input/micro_test.csv") # dataset comparing corrected data for the region from Storlei and MAT retrieved from ERA5


# Function
gen_var <- function(species_sel = "archeri",
                    sites = sites_sel, # assuming that your output files are called (for example): enbal_AU2_archeri.csv (model_site_species.csv)
                    model = "masbal",
                    variable_name = "H2O", # for water just use H2O, the function already does the addition of both; for Qgen just use the variable name "energy"
                    period = c(48, 168, 504, 720, 2160, 4320, 8760), # in hours for the rolling mean
                    threshold = 0.4, # threshold to define stress, here is for TC specifically
                    hour_threshold = 5, # consecutive hours stress
                    ystart = 1990,
                    yfinish = 2022,
                    output = "output_model_1_possum",
                    microclimate = "microclimate_possum",
                    week_stress = TRUE,
                    cumulative_stress = TRUE,
                    rolling_average = TRUE,
                    hour_stress = TRUE) {
  
  ### Start of the function
  # 
  # # read files for the specific model out (prefix) and the species of interest
  # file_names <- list.files(path = paste0("model_output/", output, "/"), pattern = paste0("^", model, ".*", species_sel, "\\.csv$"))
  # datasets <- list()
  # 
  # # Read all sites for the combination of model output and species
  # for (file_name in 1:length(file_names)) {
  #   dataset <- read.csv(paste0("model_output/", output, "/", file_names[[file_name]]))
  #   datasets[[file_name]] <- dataset
  # }
  # 
  # Retrieve microclimate variables
  met_df <- data.frame()
  soil_df <- data.frame()
  rain_df <- data.frame()
  
  for (i in 1:length(sites)) {
    load(paste0('model_output/', microclimate,'/micro_', sites[i], '_1990_2022.Rda')) # for possum sites
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
    #met$TAREF_2 <- met$TAREF - micro_adjust[i,4]
    #met$TSKYC_2 <- met$TSKYC - micro_adjust[i,4]
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
  
#   # Compile data selected
#   data <- do.call(rbind, datasets)
#   data$dates <- met$date # because I have all sites in a rbind file, so I need this date
#   data$year <- as.integer(strftime(data$dates, format = "%Y"))
#   data$week <- as.integer(strftime(data$dates, format = "%V"))
# 
#   # Conditional variation of certain parameters
#   
#   if(variable_name == "H2O"){
#     data$H2O <- (data$H2OResp_g + data$H2OCut_g)
#   }
#   
#   if(variable_name == "energy"){
#     
#     data$energy <- data$QGEN * 3.6 # hourly energy requirement in KJ
#   }
# 
#   
# # Compute datasets
#   
#   if (rolling_average) { # this computes the rolling average for the selected period above
#     rollmean_df <- data %>%
#       arrange(site, dates) %>%
#       group_by(site) %>%
#       mutate(
#         rolling_2_days = rollmeanr(!!sym(variable_name), k = period[[1]], fill = NA),
#         rolling_7_days = rollmeanr(!!sym(variable_name), k = period[[2]], fill = NA),
#         rolling_21_days = rollmeanr(!!sym(variable_name), k = period[[3]], fill = NA),
#         rolling_1_month = rollmeanr(!!sym(variable_name), k = period[[4]], fill = NA),
#         rolling_3_months = rollmeanr(!!sym(variable_name), k = period[[5]], fill = NA),
#         rolling_6_months = rollmeanr(!!sym(variable_name), k = period[[6]], fill = NA),
#         rolling_12_months = rollmeanr(!!sym(variable_name), k = period[[7]], fill = NA)
#       ) %>%
#       ungroup()
#     
#   }
#   
#   if (cumulative_stress) { # this computes the cumulative stress by multiplying the rolling average by the period
#     cumulative_df <- data %>%
#       arrange(site, dates) %>%
#       group_by(site) %>%
#       mutate(
#         cumulative_2_days = (rollmeanr(!!sym(variable_name), k = period[[1]], fill = NA)) * period[[1]],
#         cumulative_7_days = (rollmeanr(!!sym(variable_name), k = period[[2]], fill = NA)) * period[[2]],
#         cumulative_21_days = (rollmeanr(!!sym(variable_name), k = period[[3]], fill = NA)) * period[[3]],
#         cumulative_1_month = (rollmeanr(!!sym(variable_name), k = period[[4]], fill = NA)) * period[[4]],
#         cumulative_3_months = (rollmeanr(!!sym(variable_name), k = period[[5]], fill = NA)) * period[[5]],
#         cumulative_6_months = (rollmeanr(!!sym(variable_name), k = period[[6]], fill = NA)) * period[[6]],
#         cumulative_12_months = (rollmeanr(!!sym(variable_name), k = period[[7]], fill = NA)) * period[[7]]
#       ) %>%
#       ungroup()
#     
# 
#   }
#   
#   if (week_stress) { # this computes something similar to the Degree Heating Week. where we calculated the intensity of values above the threshold on a given week (on average)
#     # Threshold
#     
#     # Calculate the DHW values relative to the threshold for each site and week
#     week_stress <- data %>%
#       group_by(site) %>% 
#       mutate(threshold = threshold,
#              stress = case_when(
#                !!sym(variable_name) > threshold ~ (!!sym(variable_name) - threshold), # amount of stress
#                TRUE ~ 0 # set to zero no stress
#              )) %>% 
#       mutate(stress_3 = rollsumr(stress, k = 24*3, fill = NA, align = "right")) %>%  # 3 days
#       ungroup() 
#     
# 
#   }
#   
#   if(hour_stress){ # this calculates the number of stressful events in a week. The event is defined by the number of hours that a given variable
#     # is above the threshold. Then, we select only events with more than a given amount of hours, as it is biologically more meaninful. So, the output
#     # is the number of stressful events in a given week given the threshold of stress (intensity) and duration of the stress. We also compute the longest 
#     # stressful event each week
#     
#     combinations <- expand.grid(site = unique(data$site),
#                                 year = unique(data$year),
#                                 week = unique(data$week))
#     
# 
#     events <- data %>%
#       mutate(date = as.Date(dates)) %>%
#       group_by(site, year, week, date) %>%
#       mutate(event = ifelse(!!sym(variable_name) >= threshold, 1, 0)) %>%
#       arrange(dates) %>%
#       group_by(site, year, week, date) %>%
#       mutate(consecutive_hours = cumsum(event)) %>%
#       group_by(site, year, week, consecutive_group = cumsum(consecutive_hours < hour_threshold)) %>%
#       filter(consecutive_hours >= hour_threshold) %>%
#       ungroup() %>%
#       group_by(site, year, week) %>%
#       summarize(events_per_week = sum(event),
#                 longest_run = max(consecutive_hours)) %>%
#       right_join(combinations, by = c("site", "year", "week")) %>%
#       mutate(events_per_week = replace_na(events_per_week, 0),
#              longest_run = replace_na(longest_run, 0)) %>%
#       arrange(site, year, week)
#     
#     
#     
#   }

  return(list(
    # week_stress_df = week_stress,
    # cumulative_stress_df = cumulative_df,
    # rollmean_stress_df = rollmean_df,
    microclimate_df = met_df,
    soil_df = soil_df,
    rain_df = rain_df,
    # full_data = data,
    # hour_stress_df = events,
    species_sel = species_sel,
    sites = sites,
    model = model,
    variable_name = variable_name,
    period = period,
    threshold = threshold,
    hour_threshold = hour_threshold,
    ystart = ystart,
    yfinish = yfinish,
    # combination = combinations,
    output = output
  ))

}


# test some stuff 

out <- gen_var()

out$microclimate_df$hour <- hour(out$microclimate_df$date)

out$microclimate_df[1:24,] %>% ggplot(aes(hour, TAREF))+
  geom_point()

micro_df <- out$microclimate_df
adjust <- read.csv("Data/data_input/micro_test.csv")
adjust <- adjust %>% dplyr::select(site, max_adj, min_adj)

#adjust <- adjust %>% filter(site ==  sites[1,1])

micro_ad <- left_join(micro_df, adjust)

adjusted_temperature <- micro_ad %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),  # Convert date to POSIXct format
         hour_of_day = hour(date),  # Extract the hour from the date
         TAREF = ifelse(hour_of_day >= 8 & hour_of_day < 20, TAREF - max_adj, TAREF - min_adj))

tail(adjusted_temperature)

adjusted_temperature[1:24,] %>% ggplot(aes(hour, TAREF))+
  geom_point()+
  geom_point(data = out$microclimate_df[1:24,], aes(hour, TAREF), col = "red")

view(adjusted_temperature %>% group_by(site) %>% summarise(mat = mean(TAREF, na.rm = T)))

#save.image(paste0("model_output/",out$species_sel,"_", out$output,"_",out$model,"_",out$variable_name,".RData"))



# Climate change - temperature - plot -------------------------------------

met <- out$microclimate_df

met$year <- year(met$date)

met$month <- month(met$date)

met2 <- met %>% group_by(site, year, month) %>%
  summarise(TAREF = mean(TAREF, na.rm = T)) %>%
  arrange(site, year, month) %>% ungroup() %>%
  group_by(site) %>% mutate(roll_old = rollmeanr(TAREF, k = 36, fill = NA)) # 3 years window average

mat <- met %>% group_by(year, site) %>% summarise(mat = mean(TAREF, na.rm = T))


(one <- met2 %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = roll_old), col = "red")+
  facet_wrap(~site, scales = "free")+
  theme_classic())



# -------------------------------------------------------------------------



met <- adjusted_temperature

met$year <- year(met$date)

met$month <- month(met$date)

met2 <- met %>% group_by(site, year, month) %>%
  summarise(TAREF = mean(TAREF, na.rm = T)) %>%
  arrange(site, year, month) %>% ungroup() %>%
  group_by(site) %>% mutate(roll_old = rollmeanr(TAREF, k = 36, fill = NA)) # 3 years window average

mat <- met %>% group_by(year, site) %>% summarise(mat = mean(TAREF, na.rm = T))


(two <- met2 %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = roll_old), col = "black")+
  facet_wrap(~site, scales = "free")+
  theme_classic())
