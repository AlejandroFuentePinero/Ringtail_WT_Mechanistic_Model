library(tidyverse)
library(NicheMapR)

# Observed
krock <- read_csv("Data/data_input/chamber_grtp.csv")
obs_TCs <- krock$`Body temp oC` # evaporative water loss
obs_QGEN <- ((1000) * (krock$`RMR kJ/d`) / (24*60*60)) # metabolic rate
obs_H2O <- (krock$`EvapWaterloss mg/min`)*0.06 # body temperature
obs_mv <- krock$`MinuteVolume ml/min`


# Call predictions --------------------------------------------------------

# Create an empty list to store the datasets
datasets <- list()

# Define the prefixes for the file names
prefixes <- c("enbal", "masbal", "morph", "treg")

# Loop through the prefixes
for (prefix in prefixes) {
  # Get the file names that start with the prefix
  file_names <- list.files(path = "model_output/", pattern = paste0("^", prefix))
  
  # Read each dataset and store it in the list
  for (file_name in file_names) {
    dataset <- read.csv(paste0("model_output/", file_name))
    datasets[[file_name]] <- dataset
  }
}

# Compile microclimate ----------------------------------------------------

sites <- read.csv("Data/climate/Leahy_Atherton_Carbine_HOBO_2019-21.csv")
sites <- aggregate(sites[, 6:7], by = list(sites$Site), FUN = max)
nsites <- length(unique(sites$Group.1))


nam_met <- c("DOY", "TIME", "TALOC", "TAREF", "RHLOC", 
             "RH", "VLOC", "VREF", "SNOWMELT", "POOLDEP", 
             "PCTWET", "ZEN", "SOLR", "TSKYC", "DEW", 
             "FROST","SNOWFALL", "SNOWDEP",  "SNOWDENS")

nam_soil <- c("DOY", "TIME", "D0cm", "D2.5cm", "D5cm","D10cm", 
              "D15cm", "D20cm", "D30cm", "D50cm","D100cm", "D200cm")

metout <- data.frame(matrix(ncol = 19, nrow = 0))
soil <- data.frame(matrix(ncol = 12, nrow = 0))

colnames(metout) <- nam_met
colnames(soil) <- nam_soil

for(i in 1:nsites){
  load(paste0('model_output/micro_', sites[i, 1], '.Rda')) 
  # Save met
  out_met <- as.data.frame(micro$shadmet)
  out_met$site <- sites[i,1]
  metout <- rbind(metout,out_met)
  # Save soil
  out_soil <- as.data.frame(micro$shadsoil)
  out_soil$site <- sites[i,1]
  soil <- rbind(soil, out_soil)
  
}

rm(micro) # free memory
rm(out_met) # free memory
rm(out_soil) # free memory


# Combine the datasets using rbind
enbal <- do.call(rbind, datasets[grep("^enbal", names(datasets))])
masbal <- do.call(rbind, datasets[grep("^masbal", names(datasets))])
morph <- do.call(rbind, datasets[grep("^morph", names(datasets))])
treg <- do.call(rbind, datasets[grep("^treg", names(datasets))])

rm(datasets) # free memory
rm(dataset) # free memory

# Assign sequential row numbers as row names
row.names(enbal) <- 1:nrow(enbal)
row.names(masbal) <- 1:nrow(masbal)
row.names(morph) <- 1:nrow(morph)
row.names(treg) <- 1:nrow(treg)


treg %>% ggplot(aes(dates, TC))+
  geom_point()+
  facet_grid(species~site)
