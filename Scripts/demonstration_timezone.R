
microclimate = "microclimate_possum"
sites <- "AU10A"
load(paste0('model_output/', microclimate,'/micro_', sites, '_1990_2022.Rda')) # for possum sites


m1 <- as.data.frame(micro$shadmet)

m1$date <- micro$dates # black

head(m1)
# Adjust date ( this won't be needed for future models, as the date is already)
dates2 <- micro$dates
gmtzone <- ""
tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
attr(dates2, "tzone") <- tz
dates3 <- dates2 + (10*3600)

m1$date2 <- dates2 # blue
head(m1)

m1$dates3 <- dates3 # red
head(m1)
m1$h1 <- hour(m1$date)
m1$h2 <- hour(m1$date2)
m1$h3 <- hour(m1$dates3)


m1[1:24,] %>% ggplot()+
  geom_point(aes(h1, SOLR), col = "black", size = 2)+
  geom_point(aes(h2, SOLR), col = "blue", size = 2)+
  geom_point(aes(h3, SOLR), col = "red", size = 2)+
  theme_classic()


