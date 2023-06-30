library(tidyverse)
library(NicheMapR)
library(zoo)
library(data.table)
library(ISOweek)

# Cheatsheet
species <- c("archeri", "cinereus", "lemuroides", "herbertensis")
prefixes <- c("enbal", "masbal", "morph", "treg")
sites <- c("AU2", "AU6", "AU10", "CU1", "CU6", "CU12")
variable_names <- c("TC", "H20", "energy")

gen_var <- function(species_sel = "archeri",
                    sites = c("AU2", "AU6", "AU10", "CU1", "CU6", "CU12"),
                    model = "treg",
                    variable_name = "TC", # for water just use H2O, the function already does the addition of both
                    period = c(48, 168, 504, 720, 2160, 4320, 8760), # in hours for the rolling mean
                    threshold = 37, # threshold to define stress, here is for TC specifically
                    hour_threshold = 5, # consecutive hours stress
                    ystart = 2019,
                    yfinish = 2021,
                    week_stress = TRUE,
                    cumulative_stress = TRUE,
                    rolling_average = TRUE,
                    hour_stress = TRUE) {
  
  ### Start of the function
  
  # read files for the specific model out (prefix) and the species of interest
  file_names <- list.files(path = "model_output/", pattern = paste0("^", model, ".*", species_sel, "\\.csv$"))
  datasets <- list()
  
  # Read all sites for the combination of model output and species
  for (file_name in 1:length(file_names)) {
    dataset <- read.csv(paste0("model_output/", file_names[[file_name]]))
    datasets[[file_name]] <- dataset
  }
  
  # Retrieve microclimate variables
  met_df <- data.frame()
  soil_df <- data.frame()
  rain_df <- data.frame()
  
  for (i in 1:length(sites)) {
    load(paste0("model_output/micro_", sites[i], ".Rda"))
    # Met
    met <- as.data.frame(micro$shadmet)
    met$site <- sites[i]
    met$date <- micro$dates
    met_df <- rbind(met_df, met)
    # Soil
    soil <- as.data.frame(micro$shadsoil)
    soil$site <- sites[i]
    soil$date <- micro$dates
    soil_df <- rbind(soil_df, met)
    # Rain
    rain <- tibble(rain = micro$RAINFALL)
    dstart <- as.Date(paste0("01/01/", ystart), format = "%d/%m/%Y")
    dfinish <- as.Date(paste0("31/12/", yfinish), format = "%d/%m/%Y")
    dates <- seq(dstart, dfinish, by = "day")
    rain$date <- dates
    rain$doy <- yday(rain$date)
    rain$year <- year(rain$date)
    rain$site <- sites[i]
    rain$rain_bin <- ifelse(rain$rain>0,1,0)
    rain_df <- rbind(rain_df, rain)
  }
  
  # Compile data selected
  data <- do.call(rbind, datasets)
  data$year <- as.integer(strftime(data$dates, format = "%Y"))
  data$week <- as.integer(strftime(data$dates, format = "%V"))

  # Conditional variation of certain parameters
  
  if(variable_name == "H20"){
    data$H20 <- (data$H2OResp_g + data$H2OCut_g)
  }
  
  if(variable_name == "energy"){
    
    data$energy <- data$QGEN * 3.6 # hourly energy requirement in KJ
  }

  
# Compute datasets
  
  if (rolling_average) { # this computes the rolling average for the selected period above
    rollmean_df <- data %>%
      arrange(site, dates) %>%
      group_by(site) %>%
      mutate(
        rolling_2_days = rollmeanr(!!sym(variable_name), k = period[[1]], fill = NA),
        rolling_7_days = rollmeanr(!!sym(variable_name), k = period[[2]], fill = NA),
        rolling_21_days = rollmeanr(!!sym(variable_name), k = period[[3]], fill = NA),
        rolling_1_month = rollmeanr(!!sym(variable_name), k = period[[4]], fill = NA),
        rolling_3_months = rollmeanr(!!sym(variable_name), k = period[[5]], fill = NA),
        rolling_6_months = rollmeanr(!!sym(variable_name), k = period[[6]], fill = NA),
        rolling_12_months = rollmeanr(!!sym(variable_name), k = period[[7]], fill = NA)
      ) %>%
      ungroup()
    
  }
  
  if (cumulative_stress) { # this computes the cumulative stress by multiplying the rolling average by the period
    cumulative_df <- data %>%
      arrange(site, dates) %>%
      group_by(site) %>%
      mutate(
        cumulative_2_days = (rollmeanr(!!sym(variable_name), k = period[[1]], fill = NA)) * period[[1]],
        cumulative_7_days = (rollmeanr(!!sym(variable_name), k = period[[2]], fill = NA)) * period[[2]],
        cumulative_21_days = (rollmeanr(!!sym(variable_name), k = period[[3]], fill = NA)) * period[[3]],
        cumulative_1_month = (rollmeanr(!!sym(variable_name), k = period[[4]], fill = NA)) * period[[4]],
        cumulative_3_months = (rollmeanr(!!sym(variable_name), k = period[[5]], fill = NA)) * period[[5]],
        cumulative_6_months = (rollmeanr(!!sym(variable_name), k = period[[6]], fill = NA)) * period[[6]],
        cumulative_12_months = (rollmeanr(!!sym(variable_name), k = period[[7]], fill = NA)) * period[[7]]
      ) %>%
      ungroup()
    

  }
  
  if (week_stress) { # this computes something similar to the Degree Heating Week. where we calculated the intensity of values above the threshold on a given week (on average)
    # Threshold
    
    # Calculate the weekly mean temperature for each site
    weekly_mean <- data %>%
      group_by(site, year, week) %>%
      summarise(max_var = max(!!sym(variable_name), na.rm = TRUE)) # maximum stress during the week to calculate the relative stress compared to long-term average
    
    # Calculate the DHW values relative to the threshold for each site and week
    week_stress <- weekly_mean %>%
      group_by(site, year, week) %>%
      mutate(dhw = cumsum(pmax(max_var - threshold, 0))) # only positive anomalies with pmax (so values above threshold)
    

  }
  
  if(hour_stress){ # this calculates the number of stressful events in a week. The event is defined by the number of hours that a given variable
    # is above the threshold. Then, we select only events with more than a given amount of hours, as it is biologically more meaninful. So, the output
    # is the number of stressful events in a given week given the threshold of stress (intensity) and duration of the stress. We also compute the longest 
    # stressful event each week
    
    combinations <- expand.grid(site = unique(data$site),
                                year = unique(data$year),
                                week = unique(data$week))
    

    events <- data %>%
      mutate(date = as.Date(dates)) %>%
      group_by(site, year, week, date) %>%
      mutate(event = ifelse(!!sym(variable_name) >= threshold, 1, 0)) %>%
      arrange(dates) %>%
      group_by(site, year, week, date) %>%
      mutate(consecutive_hours = cumsum(event)) %>%
      group_by(site, year, week, consecutive_group = cumsum(consecutive_hours < hour_threshold)) %>%
      filter(consecutive_hours >= hour_threshold) %>%
      ungroup() %>%
      group_by(site, year, week) %>%
      summarize(events_per_week = sum(event),
                longest_run = max(consecutive_hours)) %>%
      right_join(combinations, by = c("site", "year", "week")) %>%
      mutate(events_per_week = replace_na(events_per_week, 0),
             longest_run = replace_na(longest_run, 0)) %>%
      arrange(site, year, week)
    
    
    
  }

  return(list(
    week_stress_df = week_stress,
    weekly_mean = weekly_mean,
    cumulative_stress_df = cumulative_df,
    rollmean_stress_df = rollmean_df,
    microclimate_df = met_df,
    soil_df = soil_df,
    rain_df = rain_df,
    full_data = data,
    hour_stress_df = events,
    species_sel = species_sel,
    sites = sites,
    model = model,
    variable_name = variable_name,
    period = period,
    threshold = threshold,
    hour_threshold = hour_threshold,
    ystart = ystart,
    yfinish = yfinish,
    combination = combinations
  ))

}


# test some stuff 

out <- gen_var()

out$full_data %>% ggplot(aes(x = dates, y = TC))+
  geom_point()+
  geom_hline(yintercept = 37, col = "red", linewidth = 1)+
  facet_wrap(~site)

# view(out$hour_stress_df)
# view(out$week_stress_df)
# view(out$rain_df)
# view(out$combination)
# view(out$full_data)

out$rain_df %>% ggplot(aes(date, rain))+
  geom_point(size = 2)+
  facet_wrap(~site)

out$hour_stress_df %>% group_by(site, year) %>% summarise(sum = sum(events_per_week))

sum(is.na(unique(out$full_data$dates)))
sum(is.na(unique(out$full_data$week)))
sum(is.na(unique(out$full_data$year)))

