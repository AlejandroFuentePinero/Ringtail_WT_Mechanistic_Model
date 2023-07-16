#####################################################################
# Metabolic chamber simulation for Green ringtail possums.          #
# Here I am comparing the metabolic response predicted by NicheMapR #
# to the observed in Krockenberger et al 2012.                      #
#####################################################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(NicheMapR)
library(patchwork)

# Working directory -------------------------------------------------------

#setwd("/Users/alejandrofp/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Data/data_input")

# Custom function ---------------------------------------------------------

source("Scripts/endoR_devel_grtp_20230628_AF.R") # Updated to latest NicheMapR version
#source("Scripts/AF_thermoreg_endoR_devel_grtp_20230621.R") # Updated to latest NicheMapR version with thermoreg happening simultaneously until panting

# Load data ---------------------------------------------------------------

krock <- read_csv("Data/data_input/chamber_grtp.csv")
fur <- read_csv("Data/data_input/fur_dataset.csv")
fur_all<-read_csv("Data/data_input/fur_data_all_species.csv")

# Plots from Krockenberger et al. 2012 ------------------------------------

rmr <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = ((1000) * (krock$`RMR kJ/d`) / (24*60*60)),col = `Possum ID`))+
  geom_point(size = 1, shape = 21, stroke = 2)+
   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)")+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12, colour = "black"))

bt <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = `Body temp oC`, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

ewl <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = ((`EvapWaterloss mg/min`)*0.06), col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Evaporative water loss (g/hour)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

rr <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = (`RespRate hz`)*60, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_line()+
    labs(x = "Ambient temperature (°C)", y = "Respiration rate (breaths/min)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

tv <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = `TidalVolume ml`, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Tidal volume (ml)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

mv <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = `MinuteVolume ml/min`, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_line()+
    labs(x = "Ambient temperature (°C)", y = "Minute volume (ml/min)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

# Individual possum plots compared to their mean --------------------------

krock_summary <- krock[,-1] %>% group_by(`Temp-category`) %>% summarise_all(mean)

rmr_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = ((1000) * (`RMR kJ/d`) / (24*60*60))))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

rmr_plot <- rmr + rmr_sum

bt_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `Body temp oC`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))
  
bt_plot <- bt + bt_sum

ewl_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = ((`EvapWaterloss mg/min`)*0.06)))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Evaporative water loss (g/hour)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

ewl_plot <- ewl + ewl_sum

rr_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = (`RespRate hz`)*60))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_line()+
    labs(x = "Ambient temperature (°C)", y = "Respiration rate (breaths/min)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

rr_plot <- rr + rr_sum

tv_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `TidalVolume ml`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Tidal volume (ml)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

tv_plot <- tv + tv_sum

mv_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `MinuteVolume ml/min`))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  geom_line()+
  labs(x = "Ambient temperature (°C)", y = "Minute volume (ml/min)")+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"))
mv_plot <- mv + mv_sum

# Environment -------------------------------------------------------------

TAs <- krock$`Temp-Ambient`
VEL <- 0.01 # from file "Copy of Ellipsoid model_heatstress3_green ringtails.xls"
hum <- as.data.frame(TAs)
hum <- hum %>% 
mutate(hum = case_when( # humidity values obtained from file "Copy of Ellipsoid model_heatstress3_green ringtails.xls)
   TAs < 7 ~ 60,
   TAs > 7 & TAs < 9 ~ 55,
   TAs > 9 & TAs < 11 ~ 50,
   TAs > 11 & TAs < 12 ~ 45,
   TAs > 12 & TAs < 15 ~ 40,
   TAs > 15 & TAs < 17 ~ 35,
   TAs > 17 ~ 30))

hum <- as.vector(hum$hum) # (CHECK with Krock)
#hum <- 40
# Core temperature --------------------------------------------------------

TCs <- krock$`Body temp oC`
#TC_MAX <- 40.8 # from Krockenberger et al 2012
TC_MAXs <- TCs
TC_INC <- 0

# Size and shape ----------------------------------------------------------

AMASS <- fur[[25,6]] # mass (kg) from Krockenberger et al. 2012
SHAPE_B <- fur[[39,6]] # start off near to a sphere (-)
SHAPE_B_MAX <- fur[[40,6]] # maximum ratio of length to width/depth
UNCURL <- 0.1 # (DEFAULT) allows the animal to uncurl to SHAPE_B_MAX, the value being the increment SHAPE_B is increased per iteration
SHAPE <- 4 # (DEFAULT) use ellipsoid geometry
#SAMODE <- 2 # (DEFAULT) (2 is mammal, 0 is based on shape specified in GEOM)
SAMODE <- 0 # (DEFAULT) (2 is mammal, 0 is based on shape specified in GEOM)
PVEN <- fur_all[[1,6]]
PVEN <- 0.5 #Adjust to incorporate limbs in ventral surface - need to update fur properties to capture this
PCOND <- 0.07 #Ask Andrew

# Fur properties ----------------------------------------------------------

DHAIRD <- fur[[12,6]] # hair diameter, dorsal (m)
DHAIRV <- fur[[13,6]] # hair diameter, ventral (m)
LHAIRD <- fur_all[[2,5]] # hair length, dorsal (m)
LHAIRV <- fur_all[[4,5]] # hair length, ventral (m)
ZFURD <- fur_all[[1,5]] # fur depth, dorsal (m)
ZFURV <- fur_all[[3,5]] # fur depth, ventral (m)
RHOD <- fur[[16,6]] # hair density, dorsal (1/m2)
RHOV <- fur[[19,6]] # hair density, ventral (1/m2)
REFLD <- 0.248 # (DEFAULT) fur reflectivity dorsal (fractional, 0-1)
REFLV <- 0.351 # (DEFAULT) fur reflectivity ventral (fractional, 0-1)

# Physiological responses -------------------------------------------------

PCTWET <- 1.5 # (CHECK) base skin wetness (%) (10% of the maximum?) ###!!!!!### I changed this from 1.5 -> 1.7
PCTWET_MAX <- fur[[37,6]] # maximum skin wetness (%)
PCTWET_INC <- 0.25 # (DEFAULT) intervals by which skin wetness is increased (%)  
PCTBAREVAP <- fur[[38,6]]
PCTBAREVAP<-3 # check this with AK
#Q10s <- rep(1,length(TAs)) 
#Q10s[TAs >= 30] <- fur[[42,6]]
#Q10 <- fur[[42,6]]
Q10 <- 1.5 #This is default, but lower than calculated values
#Q10 <- (255.59/212.35)^(10/(37.99-36.685)) # conversion of metabolic rate to Wats
QBASAL <- fur[[41,6]] # (CHECK) basal heat generation (W)
DELTARs <- rep(5,length(TAs)) # (DEFAULT) offset between air temperature and breath (°C)

#Also check setting this based on observed relationship in GGs
# EXP_TEMP<-0.76*TAs + 11.6 # at 10-25deg, exp = 0.76*TA - 11.6, 25-40 = 0.35x + 21.25
# EXP_TEMP[TAs>=25]<-0.35 * TAs[TAs>=25] + 21.35
# DELTARs<-EXP_TEMP - TAs
# 
# EXP_TEMP<-endo.out_devel1$treg.TLUNG
# DELTARs<-EXP_TEMP - TAs

#Nat to check feasibility of making DELTAR always vary with predicted TLUNG

EXTREF <- 20 # (DEFAULT) O2 extraction efficiency (%)
PANT_INC <- 0.05 # (DEFAULT) turns on panting, the value being the increment by which the panting multiplier is increased up to the maximum value, PANT_MAX
PANT_MAX <- 3 # maximum panting rate - multiplier on air flow through the lungs above that determined by metabolic rate ###!!!!!### I changed this to +2
PANT_MULT <- 0 # (DEFAULT) multiplier on basal metabolic rate at maximum panting level
AK1 <- fur[[43,6]]
AK1_MAX<- fur[[45,6]]
AK1_INC<- fur[[44,6]]

################################
# 1.- RUN ENDO_R PROVIDING:    #
#     * CORE TEMPERATURE       #
#     * AMBIENT TEMPERATURE    #
#     * METABOLIC RATE         #
################################

# Run using mass of observed individuals + estimate metabolic rate based on allometric relationship

AMASSs<-krock$Mass

a<-(231*1000/(24*60*60))/1.17^0.737

QBASALs<-a*krock$Mass0.737


# Run end Or ---------------------------------------------------------------

endo.out_devel_run1 <- lapply(1:length(TAs), function(x) {
  endoR_devel_grtp(
        # ENVIRONMENT
        TA = TAs[x], VEL = VEL, RH = hum[x], # OPTION 1: DYNAMIC HUMIDITY
        #TA = TAs[x], VEL = VEL, RH = 10, # OPTION 2: STATIC HUMIDITY 
        # CORE TEMPERATURE
        #TC = TCs[x], TC_MAX = TCs[x], TC_INC = TC_INC, # OPTION 1: TC PER OBSERVATION
        TC = fur[[34,6]], TC_MAX = fur[[35,6]], TC_INC = 0.02, # OPTION 2: AVERAGE TC; TC_MAX = 40.8 (KROCKENBERGER ET AL 2012)
        # SIZE AND SHAPE
        AMASS = AMASSs[x], SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
        UNCURL = UNCURL, SAMODE = SAMODE, PVEN = PVEN,FATPCT = 10,
        # FUR PROPERTIES
        DHAIRV = DHAIRV, LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD,
        ZFURV = ZFURV, RHOD = RHOD, RHOV = RHOV, REFLD = REFLD, DHAIRD = DHAIRD,
        # PHYSIOLOGICAL RESPONSES
        PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, PCTWET_MAX = PCTWET_MAX,
        PCTBAREVAP = 5,  AK1 = AK1, AK1_INC = AK1_INC, AK1_MAX = AK1_MAX,
        Q10 = Q10, QBASAL = QBASALs[x], DELTAR = DELTARs[x], PANT_INC = PANT_INC, # OPTION 1: Q10 PER OBSERVATION
        #Q10 = fur[[42,6]], QBASAL = QBASAL, DELTAR = DELTAR, PANT_INC = PANT_INC, # OPTION 2: Q10 WITH THE CHANGE IN MET. RATE BETWEEN 30-35 DEG C.
        PCOND = PCOND, PANT_MAX = PANT_MAX, EXTREF = EXTREF,   PANT_MULT = PANT_MULT, SHADE = 100)
}) # run endoR across environments

# extract the output
endo.out_devel1 <- do.call("rbind", lapply(endo.out_devel_run1, data.frame))

# thermoregulation output
treg <- endo.out_devel1[, grep(pattern = "treg", colnames(endo.out_devel1))]
colnames(treg) <- gsub(colnames(treg), pattern = "treg.", replacement = "")
# morphometric output
morph <- endo.out_devel1[, grep(pattern = "morph", colnames(endo.out_devel1))]
colnames(morph) <- gsub(colnames(morph), pattern = "morph.",replacement = "")
# heat balance
enbal <- endo.out_devel1[, grep(pattern = "enbal", colnames(endo.out_devel1))]
colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.",
                        replacement = "")
# mass aspects
masbal <- endo.out_devel1[, grep(pattern = "masbal", colnames(endo.out_devel1))]
colnames(masbal) <- gsub(colnames(masbal), pattern = "masbal.",
                         replacement = "")

# Summary values ----------------------------------------------------------

# Predicted

QGEN <- enbal$QGEN # metabolic rate (W)
H2O <- masbal$H2OResp_g + masbal$H2OCut_g # g/h water evaporated
TFA_D <- treg$TFA_D # dorsal fur surface temperature
TFA_V <- treg$TFA_V # ventral fur surface temperature
TskinD <- treg$TSKIN_D # dorsal skin temperature
TskinV <- treg$TSKIN_V # ventral skin temperature
TCs <- treg$TC # core temperature
MV <- (masbal$AIR_L) / 60 * 1000 # l/h

# Observed

obs_TCs <- krock$`Body temp oC` # evaporative water loss
obs_QGEN <- ((1000) * (krock$`RMR kJ/d`) / (24*60*60)) # metabolic rate
obs_H2O <- (krock$`EvapWaterloss mg/min`)*0.06 # body temperature
obs_mv <- krock$`MinuteVolume ml/min`

# Comparison plos ---------------------------------------------------------

pred <- tibble(source = "NicheMapR",
               met_rate = QGEN,
               evap_water_loss = H2O,
               tc = TCs,
               mv = MV,
               air_t = TAs)
obs <- tibble(source = "Krock",
              met_rate = obs_QGEN,
              evap_water_loss = obs_H2O,
              tc = obs_TCs,
              mv = obs_mv,
              air_t = TAs)

comp <- rbind(pred, obs)
comp$spp <- "archeri"
#write.csv(comp, "Data/data_input/archeri_chamber_pred.csv")


mt_comp <- comp %>% ggplot(aes(x = air_t, y = met_rate, col = source))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)", col = "Source", title = "Green Ringtail Possum")+
  scale_color_manual(values = c("black", "red"))+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 20),
      legend.position = "none")


ewl_comp <- comp %>% ggplot(aes(x = air_t, y = evap_water_loss, col = source))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Evaporative water loss\n(g/hour)", col = "Source")+
  scale_color_manual(values = c("black", "red"))+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14), 
        legend.position = "none")

bt_comp <- comp %>% ggplot(aes(x = air_t, y = tc, col = source))+
    geom_point(shape = 21, stroke = 2)+
    #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)", col = "Source", size = "Source")+
    scale_color_manual(values = c("black", "red"))+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14), 
          legend.position = "none")

mv_comp <- comp %>% ggplot(aes(x = air_t, y = mv, col = source))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Minute volumne\n(ml/min)", col = "Source")+
    scale_color_manual(values = c("black", "red"))+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14), 
          legend.position = "none")
 

out <- (mt_comp + ewl_comp) / (bt_comp + mv_comp)
out

