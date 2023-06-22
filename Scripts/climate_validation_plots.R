library(tidyverse)
library(lubridate)

setwd("/Users/alejandrofp/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Data/climate")


# Empirical data ----------------------------------------------------------


lili <- read_csv("Leahy_Atherton_Carbine_HOBO_2019-21.csv")

lili$dmy_hm <- dmy_hm(lili$Date.Time)
lili$date <- date(lili$dmy_hm)


lili %>% filter(Vertical == "GROUND") %>% ggplot(aes(dmy_hm, Temperature))+
  geom_point()+
  facet_wrap(~Site)


# Read predicted data ERA 5, 1M, 90% shade --------------------------------


au2_2019_metout <- read.csv("era5/AU2_1M_2019/metout.csv")
au2_2019_metout$site <- "AU2"
au2_2019_metout$height_m <- 1


au2_2020_metout <- read.csv("era5/AU2_1M_2020/metout.csv")
au2_2020_metout$site <- "AU2"
au2_2020_metout$height_m <- 1


au2_2021_metout <- read.csv("era5/AU2_1M_2021/metout.csv")
au2_2021_metout$site <- "AU2"
au2_2021_metout$height_m <- 1


au6_2019_metout <- read.csv("era5/AU6_1M_2019/metout.csv")
au6_2019_metout$site <- "AU6"
au6_2019_metout$height_m <- 1


au6_2020_metout <- read.csv("era5/AU6_1M_2020/metout.csv")
au6_2020_metout$site <- "AU6"
au6_2020_metout$height_m <- 1


au6_2021_metout <- read.csv("era5/AU6_1M_2021/metout.csv")
au6_2021_metout$site <- "AU6"
au6_2021_metout$height_m <- 1


au10_2019_metout <- read.csv("era5/AU10_1M_2019/metout.csv")
au10_2019_metout$site <- "AU10"
au10_2019_metout$height_m <- 1


au10_2020_metout <- read.csv("era5/AU10_1M_2020/metout.csv")
au10_2020_metout$site <- "AU10"
au10_2020_metout$height_m <- 1


au10_2021_metout <- read.csv("era5/AU10_1M_2021/metout.csv")
au10_2021_metout$site <- "AU10"
au10_2021_metout$height_m <- 1


cu1_2019_metout <- read.csv("era5/CU1_1M_2019/metout.csv")
cu1_2019_metout$site <- "CU1"
cu1_2019_metout$height_m <- 1


cu1_2020_metout <- read.csv("era5/CU1_1M_2020/metout.csv")
cu1_2020_metout$site <- "CU1"
cu1_2020_metout$height_m <- 1


cu1_2021_metout <- read.csv("era5/CU1_1M_2021/metout.csv")
cu1_2021_metout$site <- "CU1"
cu1_2021_metout$height_m <- 1


cu6_2019_metout <- read.csv("era5/CU6_1M_2019/metout.csv")
cu6_2019_metout$site <- "CU6"
cu6_2019_metout$height_m <- 1


cu6_2020_metout <- read.csv("era5/CU6_1M_2020/metout.csv")
cu6_2020_metout$site <- "CU6"
cu6_2020_metout$height_m <- 1


cu6_2021_metout <- read.csv("era5/CU6_1M_2021/metout.csv")
cu6_2021_metout$site <- "CU6"
cu6_2021_metout$height_m <- 1


cu12_2019_metout <- read.csv("era5/CU12_1M_2019/metout.csv")
cu12_2019_metout$site <- "CU12"
cu12_2019_metout$height_m <- 1


cu12_2020_metout <- read.csv("era5/CU12_1M_2020/metout.csv")
cu12_2020_metout$site <- "CU12"
cu12_2020_metout$height_m <- 1


cu12_2021_metout <- read.csv("era5/CU12_1M_2021/metout.csv")
cu12_2021_metout$site <- "CU12"
cu12_2021_metout$height_m <- 1


pred_era5 <- rbind(au2_2019_metout, au2_2020_metout, au2_2021_metout,
                   au6_2019_metout, au6_2020_metout, au6_2021_metout,
                   au10_2019_metout, au10_2020_metout, au10_2021_metout,
                   cu1_2019_metout, cu1_2020_metout, cu1_2021_metout,
                   cu6_2019_metout, cu6_2020_metout, cu6_2021_metout,
                   cu12_2019_metout, cu12_2020_metout, cu12_2020_metout) 



pred_era5 <- pred_era5 %>% mutate(dates2 = case_when(
  TIME == 0 ~ paste(dates, "00:00:00", sep = " "),
  TRUE ~ dates
))

pred_era5$date <- date(ymd_hms(pred_era5$dates2))

pred_era5$dmy_hm <- ymd_hms(pred_era5$dates2)


# Join observed and predicted ---------------------------------------------

obs <- lili %>% filter(Vertical == "GROUND") %>% select(Site, date, dmy_hm, Temperature, RH)

pred <- pred_era5 %>% select(site, date, dmy_hm, TAREF, RH)

names(pred) <- names(obs)


obs$data <- "observed - Leahy"
pred$data <- "predicted - NicheMapR"

comp <- rbind(obs,pred)


ggplot()+
  geom_point(data = pred, color = "orange", aes(dmy_hm, Temperature), alpha = 1)+
  geom_point(data = obs, color = "forestgreen", aes(dmy_hm, Temperature), alpha = 0.2)+
  facet_wrap(~Site)+
  theme_bw()


ggplot()+
  geom_point(data = pred, color = "orange", aes(dmy_hm, RH), alpha = 1)+
  geom_point(data = obs, color = "forestgreen", aes(dmy_hm, RH), alpha = 0.2)+
  facet_wrap(~Site)+
  theme_bw()
