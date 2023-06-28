library(tidyverse)
library(patchwork)

site <- c("AU2", "AU6", "AU10", "CU1", "CU6", "CU12")

# Change species
spp <- "archeri"

plots <- list()  # Create an empty list to store individual plots

for (i in 1:length(site)) {
  load(paste0("model_output/micro_", site[i], ".Rda"))
  met <- as.data.frame(micro$shadmet)
  met$site <- site[i]
  soil <- as.data.frame(micro$shadsoil)
  soil$site <- site[i]

  
  out <- read.csv(paste0("model_output/treg_", site[i], "_", spp, ".csv"))
  out <- out[1:8760,]
  met2 <- met[1:8760,]
  soil2 <- soil[1:8760,]
  
  
  p <- ggplot(data = data.frame(x = met2$TALOC, y = out$TC), # change here the variables to plot
              aes(x = x, y = y)) +
    geom_point(shape = 21, stroke = 1) +
    labs(title = paste(site[i],"-", spp)) +
    theme_classic()+
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "black"),
          title = element_text(size = 13))
  
  plots[[i]] <- p  # Store the plot in the list
  
}

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]]




