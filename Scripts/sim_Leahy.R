library(NicheMapR)
library(scales)


setwd("/Users/alejandrofp/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Data/climate")

obs <- read.csv('Leahy_Atherton_Carbine_HOBO_2019-21.csv')
sites <- aggregate(obs[, 6:7], by = list(obs$Site), FUN = max)
obs$datetime <- as.POSIXct(obs$Date.Time, format = "%d/%m/%Y %H:%M")
obs <- obs[order(obs$datetime), ]

ystart <- 2019
yfinish <- 2021
sim <- FALSE
sub <- which(dates >= as.POSIXct('2020-01-01')
             & dates < as.POSIXct('2020-06-28'))
par(mfrow = c(2, 1))
par(oma = c(2, 1, 2, 2) + 0.1)
par(mar = c(3, 3, 1.5, 1) + 0.1)
par(mgp = c(2, 1, 0))

for(i in 1:nrow(sites)){
  loc <- c(sites[i, 3], sites[i, 2])
  if(sim){
    micro <- micro_era5(loc = loc, dstart = paste0('01/01/', ystart), dfinish = paste0('31/12/', yfinish), spatial = 'C:/Spatial_Data/ERA5_Australia/ERA5')
    save(micro, file = paste0('micro_', sites[i, 1], '.Rda'))
  }else{
    load(paste0('micro_', sites[i, 1], '.Rda'))
  }

  metout <- as.data.frame(micro$metout)
  dates <- micro$dates
  gmtzone <- ""
  tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
  attr(dates, "tzone") <- tz

  to_plot <- subset(obs, Site == sites[i, 1])

  # extract rainfall and simulate wet ibuttons during rainy days
  # using ectotherm model
  RAINFALL <- micro$RAINFALL
  rainseq <- seq(1, length(RAINFALL))
  rainhr <- cbind(rep(rainseq, 24), rep(RAINFALL, 24))
  rainhr <- rainhr[order(rainhr[, 1]), ]
  rainhr <- rainhr[, 2]
  pct_wet <- rep(0, length(rainhr))
  pct_wet[rainhr > 0] <- 80
  # make ibutton in shade
  micro$metout <- micro$shadmet
  micro$soil <- micro$shadsoil
  ecto <- ectotherm(minshades = rep(80, length(RAINFALL)), pct_wet = pct_wet, live = 0)
  environ <- as.data.frame(ecto$environ)
  plot(dates[sub], metout$TAREF[sub], type = 'l', ylim = c(0, 45))
  points(dates, environ$TC, type = 'l', col = alpha('blue', 0.5))
  points(to_plot$datetime, to_plot$Temperature, type = 'l', col = alpha('red', 0.5))
  points(micro$dates2, micro$RAINFALL/10, type = 'h', col = 'grey')
  plot(dates[sub], metout$RH[sub], type = 'l', ylim = c(0, 100))
  points(to_plot$datetime, to_plot$RH, type = 'l', col = alpha('red', 0.5))
  points(micro$dates2, micro$RAINFALL, type = 'h', col = 'grey')
}
