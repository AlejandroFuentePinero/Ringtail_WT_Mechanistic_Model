library(tidyverse)
library(lubridate)

sites <- read.csv('Data/data_input/sites_geolocation_possum.csv')

sites <- sites %>% filter(site %in% c("AU6A", "AU10A", "AU4A", "AU7A", "AU7B", "AU8A", "AU8B", "AU10B",
                                      "AUAC"))

met_df <- data.frame()


for (i in 1:nrow(sites)) {
  
  site_id <- sites[i,1]
  
  load(paste0('microclimate/era5_afp/micro_',site_id,'_test.Rda')) # for possum sites
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
  met$site <- sites[i,1]
  met$date <- dates
  met_df <- rbind(met_df, met)
}

pred <- met_df
pred$year <- year(pred$date)
pred <- pred %>% dplyr::select(site, date, TAREF)
names(pred) <- c("site", "date", "temp")
pred$ver <- "predicted"
obs <- read.csv('Data/climate/Leahy_Atherton_Carbine_HOBO_2019-21.csv')
obs$date <- dmy_hm(obs$Date.Time)
obs <- obs %>% mutate(year = year(dmy_hm(Date.Time)))
obs <- obs %>% filter(year %in% c(2019, 2020)) %>% mutate(site = case_when(
  Site == "AU10" ~ "AU10A",
  Site == "AU6" ~ "AU6A",
  Site == "CU12" ~ "CU12A",
  Site == "CU6" ~ "CU6A",
  TRUE ~ Site
))

obs <- obs %>% dplyr::select(site, date, Temperature)
names(obs) <- c("site", "date", "temp")
obs$ver <- "observed"

comp <- rbind(obs, pred)

comp %>% filter(site %in% c("AU6A", "AU10A")) %>% ggplot(aes(date, temp, col = ver, linewidth = ver))+
  scale_linewidth_manual(values = c(2,0.5))+
  geom_line()+
  facet_wrap(~site)


met_df %>% group_by(site) %>% summarise(mat = mean)
