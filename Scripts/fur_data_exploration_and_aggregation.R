library(tidyverse)
library(raster)
library(rgdal)
library(ozmaps)
library(patchwork)
library(lubridate)

setwd("/Users/alejandrofp/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Data/data_input")

d <- read.csv("fur_measurement_QLD_museum.csv")

d$date <- dmy(d$date)

d$year <- year(d$date)


d2 <- d[,c(1,2,3,7,8,9,11,26)]

d2 <- d2[!duplicated(d2),]


# Sample size -------------------------------------------------------------



d2 %>% ggplot(aes(x = fct_infreq(species)))+
  geom_bar(col = "black", fill = "black", alpha = 0.75)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white", size = 5)+
  labs(x = "Species", y = "Count")+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"))


# Map ---------------------------------------------------------------------


points <- d2 %>% filter(id != 38)

pere <- points %>% filter(species == "peregrinus")
arch <- points %>% filter(species == "archeri")
herb <- points %>% filter(species == "herbertensis")
lemu <- points %>% filter(species == "lemuroides")
cine <- points %>% filter(species == "cinereus")

sf_oz <- ozmap_data("country")

(pere_plot <- sf_oz %>% ggplot() + 
  geom_sf()+
  geom_point(data = pere, aes(x = long, y = lat),shape = 21,size = 1.5, stroke = 0.5, fill = "orange", col = "black")+
  scale_x_continuous(breaks = c(142, 146, 150, 154), limits = c(142,154))+
  ggtitle("peregrinus")+
  theme_classic())


(arch_plot <- sf_oz %>% ggplot() + 
    geom_sf()+
    geom_point(data = arch, aes(x = long, y = lat),shape = 21,size = 1.5, stroke = 0.5, fill = "orange", col = "black")+
    scale_x_continuous(breaks = c(144.5, 146.5), limits = c(144,147))+
    ylim(-20, -15)+
    ggtitle("archeri")+
    theme_classic())


(herb_plot <- sf_oz %>% ggplot() + 
    geom_sf()+
    geom_point(data = herb, aes(x = long, y = lat),shape = 21,size = 1.5, stroke = 0.5, fill = "orange", col = "black")+
    scale_x_continuous(breaks = c(144.5, 146.5), limits = c(144,147))+
    ylim(-20, -15)+
    ggtitle("herbertensis")+
    theme_classic())

(lemu_plot <- sf_oz %>% ggplot() + 
    geom_sf()+
    geom_point(data = lemu, aes(x = long, y = lat),shape = 21,size = 1.5, stroke = 0.5, fill = "orange", col = "black")+
    scale_x_continuous(breaks = c(144.5, 146.5), limits = c(144,147))+
    ylim(-20, -15)+
    ggtitle("lemuroides")+
    
    theme_classic())

(cine_plot <- sf_oz %>% ggplot() + 
    geom_sf()+
    geom_point(data = cine, aes(x = long, y = lat),shape = 21,size = 1.5, stroke = 0.5, fill = "orange", col = "black")+
    scale_x_continuous(breaks = c(144.5, 146.5), limits = c(144,147))+
    ylim(-20, -15)+
    ggtitle("cinereus")+
    
    theme_classic())

pere_plot + (arch_plot / herb_plot) + (lemu_plot / cine_plot)



# time series -------------------------------------------------------------


d2 %>% group_by(species, year) %>% mutate(N = n()) %>% ggplot(aes(x = year, y = species))+
  geom_point(aes(size = N), shape = 21, stroke = 2)+
  scale_x_continuous(breaks = c(1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020), limits = c(1880,2020))+
  labs(x = "Year", y = "Species")+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"))
  
# summary - spatiotemporal ------------------------------------------------

# elevation

d <- d %>% mutate(elev_cat = case_when(
  elevation <= 100 ~ 100,
  elevation > 100 & elevation <= 200 ~ 200,
  elevation > 200 & elevation <= 300 ~ 300,
  elevation > 300 & elevation <= 400 ~ 400,
  elevation > 400 & elevation <= 500 ~ 500,
  elevation > 500 & elevation <= 600 ~ 600,
  elevation > 600 & elevation <= 700 ~ 700,
  elevation > 700 & elevation <= 800 ~ 800,
  elevation > 800 & elevation <= 900 ~ 900,
  elevation > 900 & elevation <= 1000 ~ 1000,
  elevation > 1000 & elevation <= 1100 ~ 1100,
  elevation > 1100  ~ 1200
))

plot(d$elev_cat, d$elevation)

# latitude

d$lat_cat <- round(d$lat * 2) / 2

plot(d$lat_cat, d$lat)

# weights for the dorsal depth weighted average

d <- d %>% mutate(weight_contribution = case_when(
  metric == "depth" & area == "dorsal" ~ 0.6,
  metric == "depth" & area == "head" ~ 0.2,
  metric == "depth" & area ==  "side" ~ 0.3,
  TRUE ~ 1
))


# -------------------------------------------------------------------------
# Separate datasets to perfomr the weighted average and then join the datasets again

d_depth_ventral <- d %>% filter(metric == "depth" & area == "ventral")

d_depth_ventral <- d_depth_ventral %>% group_by(species, lat_cat, elev_cat, year,area, age) %>% 
  summarise(val = mean(value_mm, na.rm = TRUE), 
            pven = median(pven, na.rm = T),
            measure = "depth")

d_lenght <- d %>% filter(metric == "length" & type == "fur")


d_lenght <- d_lenght %>% group_by(species, lat_cat, elev_cat, year, area, age) %>% 
  summarise(val = mean(value_mm, na.rm = TRUE), 
            pven = median(pven, na.rm = T),
            measure = "lenght")



d_weight <- d %>% filter(metric == "depth" & area %in% c("dorsal", "head", "side"))

d_weight2 <- d_weight %>% group_by(species, lat_cat, elev_cat, year,age, id) %>% 
  summarise(val = weighted.mean(x = value_mm, w = weight_contribution, na.rm = TRUE), # weighted mean
            pven = median(pven, na.rm = T),
            measure = "depth",
            area = "dorsal") %>% 
  ungroup() %>% group_by(species, lat_cat, elev_cat, year,age) %>% 
  summarise(val = mean(val, na.rm = T), 
            pven = median(pven, na.rm = T),
            measure = "depth",
            area = "dorsal") %>% dplyr::select(names(d_depth_ventral))


d_summary <- rbind(d_weight2, d_depth_ventral, d_lenght)

d_summary2 <- d_summary %>% filter(species != "peregrinus") %>% mutate(val = median(val/1000, na.rm = T),
                                                                       nichemapr = case_when(
  area == "dorsal" & measure == "lenght" ~ "LHAIRD",
  area == "ventral" & measure == "lenght" ~ "LHAIRV",
  area == "dorsal" & measure == "depth" ~ "ZFURD",
  area == "ventral" & measure == "depth" ~ "ZFURV"
))

d_summary2 %>% ggplot(aes(year, val, col = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  facet_grid(measure ~ species, scales = "free")

d_summary2 %>% ggplot(aes(elev_cat, val, col = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  facet_grid(measure ~ species, scales = "free")

d_summary2 %>% ggplot(aes(lat_cat, val, col = area))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  facet_grid(measure ~ species, scales = "free")


fur_data_final <- d_summary %>% group_by(species, age, area, measure) %>% 
  summarise(val = median(val/1000, na.rm = T), # convert to m and take median
            pven = median(pven, na.rm = T)) %>% 
  filter(species != "peregrinus") %>%
  mutate(nichemapr = case_when(
    area == "dorsal" & measure == "lenght" ~ "LHAIRD",
    area == "ventral" & measure == "lenght" ~ "LHAIRV",
    area == "dorsal" & measure == "depth" ~ "ZFURD",
    area == "ventral" & measure == "depth" ~ "ZFURV"
  ))


fur_data_final %>% ggplot(aes(species, val, col = age))+
  geom_point(shape = 21, stroke = 2)+
  facet_wrap(~nichemapr, scales = "free")+
  theme_bw()

# write_csv(fur_data_final, "fur_data_all_species.csv")
# write_csv(d_summary2, "fur_data_lat_elev_year_segregation.csv")
