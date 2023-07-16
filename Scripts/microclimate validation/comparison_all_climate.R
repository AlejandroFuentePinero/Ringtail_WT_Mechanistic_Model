library(tidyverse)

sites <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)

site_sel <- c("AU10A", "AU6A", "CU12A", "CU6A")
site_sel <- c(sites$site)
site_sel <- c("WU9A","WU10A", "WU11A", "WU12A", "WU13A", "CU12A")
year_sel <- c(2014:2019)

# Read data
micro <- read.csv("microclimate/microclimate_generation_era5.csv")
era5_raw <- read.csv("microclimate/raw_weather_data_era5.csv")
lily <- read.csv("microclimate/lily.csv")
storlei <- read.csv("microclimate/storlei_daily.csv")

# Define dates
micro$date <- as.Date(paste(micro$year, micro$month, micro$day, sep = "-"))
era5_raw$date <- as.Date(paste(era5_raw$year, era5_raw$month, era5_raw$day, sep = "-"))
lily$date <- as.Date(paste(lily$year, lily$month, lily$day, sep = "-"))

# Filter to zoom in
micro <- micro %>% filter(year %in% c(year_sel), site %in% c(site_sel))
era5_raw <- era5_raw %>% filter(year %in% c(year_sel), site %in% c(site_sel))
lily <- lily %>% filter(year %in% c(year_sel), site %in% c(site_sel))
storlei <- storlei %>% filter(year %in% c(year_sel), site %in% c(site_sel))

# Include adjustements
adjust <- read.csv("Data/data_input/micro_test.csv")
elev <- adjust %>% dplyr::select(site, ERA5_elev)
adjust <- adjust %>% dplyr::select(site, max_adj, min_adj)
micro_adjust <- left_join(micro, adjust)

micro_adjust <- micro_adjust %>% mutate(tmin = tmin - min_adj,
                                        tmax = tmax - max_adj)

# Prepare data to be combined
micro$ver <- "micro_era5()"
era5_raw$ver <- "raw ERA5"
lily$ver <- "observed"
micro_adjust$ver <- "micro_adjusted"
storlei$ver <- "Storlei interp."

micro <- micro %>% dplyr::select(site, date, tmax, tmin, ver)
era5_raw <- era5_raw %>% dplyr::select(site, date, tmax, tmin, ver)
lily <- lily %>% dplyr::select(site, date, tmax, tmin, ver)
micro_adjust <- micro_adjust %>% dplyr::select(site, date, tmax, tmin, ver)
storlei <- storlei %>% dplyr::select(site, date, tmax, tmin, ver)

comp <- rbind(#micro, 
              #era5_raw, 
              lily, 
              micro_adjust, 
              storlei
              )

comp <- left_join(comp, elev)

# Plot comparison
comp %>% ggplot(aes(date, tmin, col = ver))+
  geom_line(linewidth = 0.5)+
  facet_wrap(~site)+
  theme_classic()

comp$ver <- factor(comp$ver,      # Reordering group factor levels
                         levels = c("observed", "micro_adjusted", "Storlei interp.", "micro_era5()","raw ERA5"))

comp %>%
  mutate(mat = ((tmax + tmin) / 2)) %>% 
  group_by(site, ver) %>% 
  summarise(mat = mean(mat), elev = mean(ERA5_elev)) %>% 
  ggplot(aes(elev, mat, col = ver, group = ver))+
  geom_point(shape = 21, stroke = 2)+
  geom_line(linewidth = 0.5)+
  geom_smooth(method = "lm", se = F)+
  labs(x = "Elevation", y = "Mean annual temperature", col = "Dataset")+
  #scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_bw()+
  theme(legend.position = c(0.85,0.25))+
  facet_wrap(~ver)
  

