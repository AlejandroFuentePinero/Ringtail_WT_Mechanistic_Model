library(NicheMapR)

sites <- read.csv('sites_geolocation_possum.csv')

ystart <- 1990
yfinish <- 2022

for(i in 1:nrow(sites)){
  loc <- c(sites[i, 3], sites[i, 2])
    micro <- micro_era5(loc = loc, dstart = paste0('01/01/', ystart), dfinish = paste0('31/12/', yfinish), spatial = 'C:/Spatial_Data/ERA5_Australia/ERA5')
    save(micro, file = paste0('micro_', sites[i, 1], '_1990_2022.Rda'))
}

