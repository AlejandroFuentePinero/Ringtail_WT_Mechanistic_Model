
# Libraries ---------------------------------------------------------------

library(NicheMapR)
library(scales)
library(tidyverse)


# EndoR function for possums ----------------------------------------------

source("Scripts/endoR_devel_grtp_20230628_AF.R") # Updated to latest NicheMapR version
#source("Scripts/AF_thermoreg_endoR_devel_grtp_20230621.R") # Updated to latest NicheMapR version with thermoreg happening simultaneously until panting


# Datasets ----------------------------------------------------------------

# Sites

sites <- read.csv("Data/data_input/sites_geolocation_possum.csv") # this is for all sites (32)
nsites <- length(unique(sites$site))


# Run only for Lily's sites for now
# sites <- read.csv("Data/climate/Leahy_Atherton_Carbine_HOBO_2019-21.csv")
# sites <- aggregate(sites[, 6:7], by = list(sites$Site), FUN = max)
# nsites <- length(unique(sites$Group.1))

# Fur data aggregated across species

fur <- read.csv("Data/data_input/fur_data_all_species.csv")

# Select Wet Tropics species and adult individuals

fur <- fur %>% dplyr::filter(species != "peregrinus") %>% dplyr::filter(age == "adult")

nspecies <- length(unique(fur$species))

species_id <- unique(fur$species)



##########################################

for(s in 1:nspecies){ # start loop for each ringtail species
  
  # Select species to loop 
  
  spp <- species_id[s]
  
  # Fur subset to include in the loop
  
  fur_sub <- subset(fur[fur$species == spp,])
  
  # Core temperature
  
  TC <- 35.8 # borrowed from archeri
  TC_MAX <- 40.8 # borrowed from archeri
  TC_INC <- 0.02
  
  # Size and shape
  
  # Mass (kg) obtained from allometric data
  
  AMASSs <- tibble(species = unique(fur$species),
                   mass = c(1.17, 1, 1.11, 0.95))
  
  sub <- subset(AMASSs[AMASSs$species == spp,]) # filter the mass dataset for the species selected in the loop iteration
  mass <- sub$mass
  
  
  SHAPE_B <- 1.01
  SHAPE_B_MAX <- 8
  UNCURL <- 0.1
  SHAPE <- 4
  SAMODE <- 0
  PVEN <- 0.5 # this includes limbs in ventral surface; to obtain the pven without limbs see fur dataset
  PCOND <- 0.07
  
  # Fur properties
  
  DHAIRD <- 9.93E-06 # borrowed from archeri
  DHAIRV <- 9.43E-06 # borrowed from archeri
  LHAIRD <- (fur_sub %>% filter(nichemapr == "LHAIRD"))[[5]]  # obtained from the museum dataset filtered above
  LHAIRV <- (fur_sub %>% filter(nichemapr == "LHAIRV"))[[5]] # obtained from the museum dataset filtered above
  ZFURD <- (fur_sub %>% filter(nichemapr == "ZFURD"))[[5]] # obtained from the museum dataset filtered above
  ZFURV <- (fur_sub %>% filter(nichemapr == "ZFURV"))[[5]] # obtained from the museum dataset filtered above
  RHOD <- 85992877.89 # borrowed from archeri
  RHOV <- 51392321.53 # borrowed from archeri
  REFLD <- 0.383 # borrowed from koala (NSW)
  REFLV <- 0.523  # borrowed from koala (NSW)
  
  # Physiological responses
  
  PCTWET <- 1.5
  PCTWET_MAX <- 40
  PCTWET_INC <- 0.25
  PCTBAREVAP <- 3
  Q10 <- 1.5 # 2 is default, but lower than calculated values
  #Q10 <- (255.59/212.35)^(10/(37.99-36.685)) # conversion of metabolic rate to Wats
  #QBASAL <- 2.673611111 # from green ringtail possums
  #DELTAR <- 5 
  
  #Nat to check feasibility of making DELTAR always vary with predicted TLUNG
  
  EXTREF <- 20 
  PANT_INC <-  0.05
  PANT_MAX <- 3 # before was 2
  PANT_MULT <- 0 
  AK1 <- 0.9
  AK1_MAX<- 2.8
  AK1_INC<- 0.1
  
  # CHECK THIS - Estimate metabolic rate based on allometric relationship
  
  a <- (231 * 1000 / (24 * 60 * 60)) / mass ^ 0.737 # borrowed from archeri
  QBASAL <- a * mass ^ 0.737
  
  
  # Microclimate ------------------------------------------------------------
  
  
  ystart <- 1990
  yfinish <- 2021
  sim <- FALSE
  
  for(i in 1:nsites){
    
    loc <- c(sites[i, 3], sites[i, 2])
    
    if(sim){ # turn on/off above
      
      micro <- micro_era5(loc = loc, 
                          dstart = paste0('01/01/', ystart), 
                          dfinish = paste0('31/12/', yfinish), 
                          spatial = 'C:/Spatial_Data/ERA5_Australia/ERA5')
      
      save(micro, file = paste0('model_output/micro_', sites[i, 1], '.Rda')) # save microclimate
      
    } # on/off buttom closes
    
    else{ # this way we save time by simulating microclimate only once
      load(paste0('model_output/microclimate_possum/micro_', sites[i, 1], '_1990_2022.Rda')) ####!!!!!!!##### modify path here
    } # conditional loop else close
    
      shademet <- as.data.frame(micro$shadmet) # extract environmental values
      soil <- as.data.frame(micro$shadsoil) # is ground temperature here?
      
      # Keep track of dates
      dates <- micro$dates
      gmtzone <- ""
      tz <- paste0("Etc/GMT", gmtzone, floor(micro$longlat[1]/15*-1))
      attr(dates, "tzone") <- tz
      
      # Extract microclimate variables for the model
      
      TAs <- shademet$TAREF # air temperature at local height (°C)
      TAREFs <- shademet$TAREF # air temperature at reference height (°C)          
      TGRDs = shademet$TAREF # ground temperature (°C) assuming relevant tlower = vegetation at airtemp
      TSKYs = shademet$TSKYC # sky temperature (°C)
      QSOLRs = shademet$SOLR # solar radiation, horizontal plane (W/m2)
      Zs = shademet$ZEN # zenith angle of sun (degrees from overhead)
      ELEV = micro$elev # elevation (m)
      ABSSB = 0.8 # solar absorptivity of substrate (fractional, 0-1)           # (CHECK THIS)
      RHs <- shademet$RH # relative humidity (%)
      VELs <- shademet$VREF # wind speed (m/s)
      
      
      
      ###!!!!#### here is the physiological element because it needs the TAs
      #Also check setting this based on observed relationship in GGs
      EXP_TEMP <- 0.76*TAs + 11.6 # at 10-25deg, exp = 0.76*TA - 11.6, 25-40 = 0.35x + 21.25
      EXP_TEMP[TAs>=25] <- 0.35 * TAs[TAs>=25] + 21.35
      DELTARs <- EXP_TEMP - TAs
      #####!!!!!!#####
      
      
      
      # ENDOR
      
      endo.out_devel_run1 <- lapply(1:length(TAs), function(x) { # run endoR per site
        endoR_devel_grtp(
          # ENVIRONMENT
          TA = TAs[x], VEL = VELs[x], RH = RHs[x], ELEV = ELEV, 
          TSKY = TSKYs[x], QSOLR = QSOLRs[x], Z = Zs[x], TGRD = TGRDs[x], ORIENT = 0, 
          FGDREF = 0.5, FSKREF = 0.5,
          # CORE TEMPERATURE
          TC = TC, TC_MAX = TC_MAX, TC_INC = TC_INC, 
          # SIZE AND SHAPE
          AMASS = mass, SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
          UNCURL = UNCURL, SAMODE = SAMODE, PVEN = PVEN,
          # FUR PROPERTIES
          DHAIRV = DHAIRV, LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD,
          ZFURV = ZFURV, RHOD = RHOD, RHOV = RHOV, REFLD = REFLD, DHAIRD = DHAIRD,
          # PHYSIOLOGICAL RESPONSES
          PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, PCTWET_MAX = PCTWET_MAX,
          PCTBAREVAP = PCTBAREVAP,  AK1 = AK1, AK1_INC = AK1_INC, AK1_MAX = AK1_MAX,
          Q10 = Q10, QBASAL = QBASAL, DELTAR = DELTARs[x], PANT_INC = PANT_INC,
          PANT_MAX = PANT_MAX, EXTREF = EXTREF,   PANT_MULT = PANT_MULT, FATPCT = 10,
          #BEHAVIOUR
          SHADE = 90)
      } # close endoR loop
      ) # run endoR across environments
      
      # extract the output
      endo.out_devel1 <- do.call("rbind", lapply(endo.out_devel_run1, data.frame))
      
      # thermoregulation output
      treg <- endo.out_devel1[, grep(pattern = "treg", colnames(endo.out_devel1))]
      colnames(treg) <- gsub(colnames(treg), pattern = "treg.", replacement = "")
      treg$dates <- dates
      treg$site <- sites[i,1]
      treg$species <- spp
      write_csv(treg, paste0('model_output/output_model_1_possum/treg_', sites[i, 1],"_", spp, '.csv')) # save output ####!!!!!!!##### modify path here
      
      # morphometric output
      morph <- endo.out_devel1[, grep(pattern = "morph", colnames(endo.out_devel1))]
      colnames(morph) <- gsub(colnames(morph), pattern = "morph.", replacement = "")
      morph$dates <- dates
      morph$site <- sites[i,1]
      morph$species <- spp
      write_csv(morph, paste0('model_output/output_model_1_possum/morph_', sites[i, 1],"_", spp, '.csv')) # save output ####!!!!!!!##### modify path here
      
      # heat balance
      enbal <- endo.out_devel1[, grep(pattern = "enbal", colnames(endo.out_devel1))]
      colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.", replacement = "")
      enbal$dates <- dates
      enbal$site <- sites[i,1]
      enbal$species <- spp
      write_csv(enbal, paste0('model_output/output_model_1_possum/enbal_', sites[i, 1],"_", spp, '.csv')) # save output ####!!!!!!!##### modify path here
      
      # mass aspects
      masbal <- endo.out_devel1[, grep(pattern = "masbal", colnames(endo.out_devel1))]
      colnames(masbal) <- gsub(colnames(masbal), pattern = "masbal.", replacement = "")
      masbal$dates <- dates
      masbal$site <- sites[i,1]
      masbal$species <- spp
      write_csv(masbal, paste0('model_output/output_model_1_possum/masbal_', sites[i, 1],"_", spp, '.csv')) # save output ####!!!!!!!##### modify path here
      
  } # sites loop closes
} # species loop close


