library(tidyverse)
library(lubridate)



# Empirical data ----------------------------------------------------------


lili <- read_csv("Data/climate/Leahy_Atherton_Carbine_HOBO_2019-21.csv")

lili$dmy_hm <- dmy_hm(lili$Date.Time)
lili$date <- date(lili$dmy_hm)


lili %>% filter(Vertical == "GROUND") %>% ggplot(aes(dmy_hm, Temperature))+
  geom_point()+
  facet_wrap(~Site)

lily_mat <- lili %>% filter(Vertical == "CANOPY", !Site %in% c("AU2", "CU1"))%>%  group_by(Site, year = year(date)) %>% summarise(mat = mean(Temperature, na.rm = T)) %>% filter(year %in% c(2019, 2020))

lily_mat$ver <- "obs"

names(lily_mat) <- c("site", "year", "mat", "ver")


# Shiny predictions - topography ------------------------------------------


au6_2019_metout <- read.csv("microclimate/era5_shiny/AU6_1M_2019/metout.csv")
au6_2019_metout$site <- "AU6"
au6_2019_metout$height_m <- 1


au6_2020_metout <- read.csv("microclimate/era5_shiny/AU6_1M_2020/metout.csv")
au6_2020_metout$site <- "AU6"
au6_2020_metout$height_m <- 1



au10_2019_metout <- read.csv("microclimate/era5_shiny/AU10_1M_2019/metout.csv")
au10_2019_metout$site <- "AU10"
au10_2019_metout$height_m <- 1


au10_2020_metout <- read.csv("microclimate/era5_shiny/AU10_1M_2020/metout.csv")
au10_2020_metout$site <- "AU10"
au10_2020_metout$height_m <- 1


cu6_2019_metout <- read.csv("microclimate/era5_shiny/CU6_1M_2019/metout.csv")
cu6_2019_metout$site <- "CU6"
cu6_2019_metout$height_m <- 1


cu6_2020_metout <- read.csv("microclimate/era5_shiny/CU6_1M_2020/metout.csv")
cu6_2020_metout$site <- "CU6"
cu6_2020_metout$height_m <- 1




cu12_2019_metout <- read.csv("microclimate/era5_shiny/CU12_1M_2019/metout.csv")
cu12_2019_metout$site <- "CU12"
cu12_2019_metout$height_m <- 1


cu12_2020_metout <- read.csv("microclimate/era5_shiny/CU12_1M_2020/metout.csv")
cu12_2020_metout$site <- "CU12"
cu12_2020_metout$height_m <- 1





shiny <- rbind(
                   au6_2019_metout, au6_2020_metout, 
                   au10_2019_metout, au10_2020_metout, 
                   cu6_2019_metout, cu6_2020_metout, 
                   cu12_2019_metout, cu12_2020_metout, cu12_2020_metout) 



shiny <- shiny %>% mutate(dates2 = case_when(
  TIME == 0 ~ paste(dates, "00:00:00", sep = " "),
  TRUE ~ dates
))

shiny$date <- date(ymd_hms(shiny$dates2))

shiny$dmy_hm <- ymd_hms(shiny$dates2)

shiny_mat <- shiny %>% group_by(site, year = year(date)) %>% summarise(mat = mean(TAREF, na.rm = T)) %>% filter(year %in% c(2019,2020))

shiny_mat$ver <- "shiny"



# Mike's prediction -------------------------------------------------------

load('model_output/microclimate_leahy/micro_AU6.Rda')
au6_micro <- as.data.frame(micro$shadmet)
au6_micro$site <- "AU6"


load('model_output/microclimate_leahy/micro_AU10.Rda')
au10_micro <- as.data.frame(micro$shadmet)
au10_micro$site <- "AU10"


load('model_output/microclimate_leahy/micro_CU6.Rda')
cu6_micro <- as.data.frame(micro$shadmet)
cu6_micro$site <- "CU6"

load('model_output/microclimate_leahy/micro_CU12.Rda')
cu12_micro <- as.data.frame(micro$shadmet)
cu12_micro$site <- "CU12"

mk <- rbind(au6_micro, au10_micro,
            cu6_micro, cu12_micro)

mk$date <- micro$dates
mk$year <- year(mk$date)

mk_mat <- mk %>% filter(year %in% c(2019, 2020)) %>% group_by(site, year) %>% summarise(mat = mean(TAREF, na.rm = T))
mk_mat$ver <- "MK"





# comp --------------------------------------------------------------------


comp <- rbind(lily_mat,
              shiny_mat,
              mk_mat)


comp %>% ggplot(aes(as.factor(year), mat, col = ver))+
  geom_point(shape = 21, stroke = 2)+
  facet_wrap(~site)+
  theme_bw()
