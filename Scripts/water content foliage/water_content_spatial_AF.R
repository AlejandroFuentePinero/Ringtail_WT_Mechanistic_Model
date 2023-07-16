library(tidyverse)

# set working directory

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/herbivory_rate_awt/herbivory_rate_awt/Data")

# Foliage chemistry data

fol <- read_csv("foliage_samples_PHD.csv")
fol <- fol %>% filter(Spp %in% c("Alphitonia petriei","Flindersia brayleyana","Melicope jonesii","Flindersia pimentaliana"))
fol <- fol %>% arrange(tree_id)
fol <- fol %>% filter(Age == "Mature")

water <- fol %>% select(site,Spp,`%humidity (corrected)`) %>% group_by(site,Spp) %>% summarise(mean_water = mean(`%humidity (corrected)`, na.rm = T),
                                                                                               sd_water = sd(`%humidity (corrected)`, na.rm = T))

water %>% ggplot(aes(reorder(site, mean_water), mean_water, col = Spp))+
  geom_point(shape = 21, stroke = 2)+
  theme_classic()
