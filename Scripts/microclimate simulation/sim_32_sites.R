options("rgdal_show_exportToProj4_warnings"="none")
library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(lubridate)
library(dplyr)
library(tidync)


# Download ERA5 for the Wet Tropics ---------------------------------------

# Only need to do this once

# uid <-  "210186"
# cds_api_key <-  "77aa6c13-d9dd-47d7-9dbc-e5052753dcf8"
# 
# ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")
# 
# xmn <- 145.15
# xmx <- 146.75
# ymn <- -19.8
# ymx <- -15.2
# 
# st_time <- lubridate::ymd("2022:01:01")
# en_time <- lubridate::ymd("2022:12:31")
# 
# file_prefix <- "ERA5"
# op <- "Spatial_Data/"
# 
# # build a request (covering multiple years)
# req <- build_era5_request(xmin = xmn, xmax = xmx,
#                           ymin = ymn, ymax = ymx,
#                           start_time = st_time,
#                           end_time = en_time,
#                           outfile_name = file_prefix)
# str(req)
# request_era5(request = req, uid = uid, out_path = op)



# Microclimate ------------------------------------------------------------

sites <- read.csv('Data/data_input/sites_geolocation_possum.csv')

ystart <- 2019
yfinish <- 2020


for(i in 1:nrow(sites)){
  
  loc <- c(sites[i, 3], sites[i, 2])
  
  site_id <- sites[i,1]
  
  # # dem
  dem <- microclima::get_dem(r = NA, lat = sites[i, 2], long = sites[i, 3], resolution = 10, zmin = 0, xdims = 1000, ydims = 1000)
  elev <- raster::extract(dem, c(sites[i, 3], sites[i, 2]))[1]
  xy <- data.frame(x = sites[i, 3], y = sites[i, 2])
  sp::coordinates(xy) = ~x + y
  sp::proj4string(xy) = "+init=epsg:4326"
  xy <- as.data.frame(sp::spTransform(xy, raster::crs(dem)))
  slope <- raster::terrain(dem, unit = "degrees")
  slope <- raster::extract(slope, xy)
  aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees")
  aspect <- raster::extract(aspect, xy)
  ha36 <- 0
  for (i in 0:35) {
    har <- microclima::horizonangle(dem, i * 10, raster::res(dem)[1])
    ha36[i + 1] <- atan(raster::extract(har, xy)) * (180/pi)
  }
  hori <- spline(x = ha36, n = 24, method =  'periodic')$y
  hori[hori < 0] <- 0
  hori[hori > 90] <- 90

  
  
  # micro
    micro <- micro_era5(loc = loc, dstart = paste0('01/01/', ystart), dfinish = paste0('31/12/', yfinish), spatial = 'Spatial_Data/era5',
                        dem = dem, slope = slope, aspect = aspect, hori = hori,
                        #dem = NA, dem.res = 10, pixels = 1000, zmin = 0, # in theory this generates the dem
                        minshade = 0, maxshade = 100)
    
    
    save(micro, file = paste0('microclimate/era5_afp/micro_',site_id,'_test.Rda'))
}
