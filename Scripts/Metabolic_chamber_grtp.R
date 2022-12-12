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

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Data/data_input")

source("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model/Scripts/endoR_devel_green_ringtail_updated.R")
# Load data ---------------------------------------------------------------

krock <- read_csv("chamber_grtp.csv")
fur <- read_csv("fur_dataset.csv")


# Krock plots -------------------------------------------------------------

(rmr <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = ((1000) * (krock$`RMR kJ/d`) / (24*60*60)),col = `Possum ID`))+
  geom_point(size = 1, shape = 21, stroke = 2)+
   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)")+
   theme_classic()+
   theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 12, colour = "black")))


(bt <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = `Body temp oC`, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black")))

(ewl <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = ((`EvapWaterloss mg/min`)*0.06), col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Evaporative water loss (g/hour)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black")))

(rr <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = (`RespRate hz`)*60, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_line()+
    labs(x = "Ambient temperature (°C)", y = "Respiration rate (breaths/min)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black")))

(tv <- krock %>% ggplot(aes(x = `Temp-Ambient`, y = `TidalVolume ml`, col = `Possum ID`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Tidal volume (ml)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black")))


# Summarise data across ambient temperature -------------------------------

krock_summary <- krock[,-1] %>% group_by(`Temp-category`) %>% summarise_all(mean)

rmr_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = ((1000) * (`RMR kJ/d`) / (24*60*60))))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

rmr + rmr_sum

bt_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `Body temp oC`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))
  
bt + bt_sum

ewl_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = ((`EvapWaterloss mg/min`)*0.06)))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Evaporative water loss (g/hour)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

ewl + ewl_sum

rr_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = (`RespRate hz`)*60))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_line()+
    labs(x = "Ambient temperature (°C)", y = "Respiration rate (breaths/min)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

rr + rr_sum

tv_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `TidalVolume ml`))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Tidal volume (ml)")+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"))

mv_sum <- krock_summary %>% ggplot(aes(x = `Temp-Ambient`, y = `MinuteVolume ml/min`))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  geom_line()+
  labs(x = "Ambient temperature (°C)", y = "Minute volume (ml/min)")+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"))
mv_sum

# Environment -------------------------------------------------------------


TAs <- sort(unique(krock$`Temp-Ambient`))
VEL <- 0.01 # from file "Copy of Ellipsoid model_heatstress3_green ringtails.xls)
#hum <- as.data.frame(TAs)
#hum <- hum %>% mutate(hum = case_when( # humidity values obtained from file "Copy of Ellipsoid model_heatstress3_green ringtails.xls)
#                 TAs < 7 ~ 60,
#                 TAs > 7 & TAs < 9 ~ 55,
#                 TAs > 9 & TAs < 11 ~ 50,
#                 TAs > 11 & TAs < 12 ~ 45,
#                 TAs > 12 & TAs < 15 ~ 40,
#                 TAs > 15 & TAs < 17 ~ 35,
#                 TAs > 17 ~ 30))
#
#hum <- as.vector(hum$hum) # (CHECK with Krock)
hum <- 40
# Core temperature --------------------------------------------------------

TC <- fur[[34,6]]
#TC_MAX <- 40.8 # from Krockenberger et al 2012
TC_MAX <- 39.8
#krock_high_T <- krock %>% filter(krock$`Temp-Ambient`>=30) # select temperatures above 30 deg C to calculate the TC_INC
#TC_INC <- summary(lm(krock_high_T$`Tb model` ~ krock_high_T$`Temp-Ambient`))$coefficients[2,1] # this is the increment by which TC is elevated at high ambient temperature
TC_INC <- 0.05


# Size and shape ----------------------------------------------------------

AMASS <- fur[[25,6]] # mass (kg) from Krockenberger et al. 2012
SHAPE_B <- fur[[39,6]] # start off near to a sphere (-)
SHAPE_B_MAX <- fur[[40,6]] # maximum ratio of length to width/depth
UNCURL <- 0.1 # (DEFAULT) allows the animal to uncurl to SHAPE_B_MAX, the value being the increment
# SHAPE_B is increased per iteration
SHAPE <- 4 # (DEFAULT) use ellipsoid geometry
SAMODE <- 2 # (DEFAULT) (2 is mammal, 0 is based on shape specified
# in GEOM)
PVEN <- fur[[22,6]]
PCOND <- 0


# Fur properties ----------------------------------------------------------

DHAIRD <- fur[[12,6]] # hair diameter, dorsal (m)
DHAIRV <- fur[[13,6]] # hair diameter, ventral (m)
LHAIRD <- fur[[1,6]] # hair length, dorsal (m)
LHAIRV <- fur[[2,6]] # hair length, ventral (m)
ZFURD <- fur[[11,6]] # fur depth, dorsal (m)
ZFURV <- fur[[10,6]] # fur depth, ventral (m)
RHOD <- fur[[16,6]] # hair density, dorsal (1/m2)
RHOV <- fur[[19,6]] # hair density, ventral (1/m2)
REFLD <- 0.248 # (DEFAULT) fur reflectivity dorsal (fractional, 0-1)
REFLV <- 0.351 # (DEFAULT) fur reflectivity ventral (fractional, 0-1)

# Physiological responses -------------------------------------------------

PCTWET <- 2 # (CHECK) base skin wetness (%) (10% of the maximum?)
PCTWET_MAX <- fur[[37,6]] # maximum skin wetness (%)
PCTWET_INC <- 0.25 # (DEFAULT) intervals by which skin wetness is increased (%)  
PCTBAREVAP <- fur[[38,6]]
#Q10s <- rep(fur[[42,6]],length(TAs)) # (CHECK)
#Q10s[TAs >= 30] <- fur[[42,6]] # (CHECK)
#Q10 <- fur[[42,6]]
Q10 <- 2
QBASAL <- fur[[41,6]] # (CHECK) basal heat generation (W)
DELTAR <- 5 # (DEFAULT) offset between air temperature and breath (°C)
EXTREF <- 20 # (DEFAULT) O2 extraction efficiency (%)
PANT_INC <- 0.05 # (DEFAULT) turns on panting, the value being the increment by which the panting multiplier
# is increased up to the maximum value, PANT_MAX
PANT_MAX <- fur[[36,6]] # maximum panting rate - multiplier on air flow through the lungs above
# that determined by metabolic rate
PANT_MULT <- 1 # (DEFAULT) multiplier on basal metabolic rate at maximum panting level
AK1 <- fur[[43,6]]
AK1_MAX<- fur[[45,6]]
AK1_INC<- fur[[44,6]]

# Run endoR ---------------------------------------------------------------

#endo.out <- lapply(1:length(TAs), function(x) {
#  endoR(TA = TAs[x], VEL = VEL, TC = TC, TC_MAX = TC_MAX, RH = hum[x],
#        AMASS = AMASS, SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
#        PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, PCTWET_MAX = PCTWET_MAX,
#        PCTBAREVAP = PCTBAREVAP, PVEN = PVEN, AK1 = AK1, AK1_INC = AK1_INC, AK1_MAX = AK1_MAX,
#        Q10 = Q10, QBASAL = QBASAL, DELTAR = DELTAR, DHAIRD = DHAIRD,
#        DHAIRV = DHAIRV, LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD,
#        ZFURV = ZFURV, RHOD = RHOD, RHOV = RHOV, REFLD = REFLD,
#        TC_INC = TC_INC, PANT_INC = PANT_INC, PANT_MAX = PANT_MAX,
#        EXTREF = EXTREF, UNCURL = UNCURL, SAMODE = SAMODE, PANT_MULT = PANT_MULT)
#}) # run endoR across environments

endo.out_devel <- lapply(1:length(TAs), function(x) {
  endoR_devel_grt(TA = TAs[x], VEL = VEL, TC = TC, TC_MAX = TC_MAX, RH = hum, #RH = hum[x],
        AMASS = AMASS, SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
        PCTWET = PCTWET, PCTWET_INC = PCTWET_INC/2, PCTWET_MAX = PCTWET_MAX,
        PCTBAREVAP = 5, PVEN = PVEN, AK1 = AK1, AK1_INC = AK1_INC/2, AK1_MAX = AK1_MAX,
        Q10 = Q10, QBASAL = QBASAL, DELTAR = DELTAR, DHAIRD = DHAIRD,
        DHAIRV = DHAIRV, LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD,
        ZFURV = ZFURV, RHOD = RHOD, RHOV = RHOV, REFLD = REFLD,
        TC_INC = TC_INC/2, PANT_INC = PANT_INC/2, PANT_MAX = PANT_MAX,
        EXTREF = EXTREF, UNCURL = UNCURL/2, SAMODE = SAMODE, PANT_MULT = PANT_MULT)
}) # run endoR across environments

# extract the output
endo.out_devel1 <- do.call("rbind", lapply(endo.out_devel, data.frame))

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
# respiration rate
# tidal volume

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

(mt_comp <- comp %>% ggplot(aes(x = air_t, y = met_rate, col = source))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)", col = "Source")+
  scale_color_manual(values = c("black", "red"))+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)))


(ewl_comp <- comp %>% ggplot(aes(x = air_t, y = evap_water_loss, col = source))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Evaporative water loss (g/hour)", col = "Source")+
  scale_color_manual(values = c("black", "red"))+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)))

(bt_comp <- comp %>% ggplot(aes(x = air_t, y = tc, col = source))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Body temperature (°C)", col = "Source")+
    scale_color_manual(values = c("black", "red"))+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)))

(mv_comp <- comp %>% ggplot(aes(x = air_t, y = mv, col = source))+
    geom_point(size = 1, shape = 21, stroke = 2)+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
    labs(x = "Ambient temperature (°C)", y = "Minute volumne (ml/min)", col = "Source")+
    scale_color_manual(values = c("black", "red"))+
    theme_classic()+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)))

#edit(endoR_devel)

plot(endo.out_devel1$treg.SHAPE_B ~ TAs)
plot(endo.out_devel1$treg.PANT ~ TAs)
plot(endo.out_devel1$ ~ TAs)
plot(endo.out_devel1$treg.K_FLESH ~ TAs)
plot(endo.out_devel1$treg.PCTWET ~ TAs)
plot(endo.out_devel1$treg.K_FUR_V ~ TAs)
plot(endo.out_devel1$treg.K_FUR ~ TAs)
plot(endo.out_devel1$treg.K_FUR_D ~ TAs)
