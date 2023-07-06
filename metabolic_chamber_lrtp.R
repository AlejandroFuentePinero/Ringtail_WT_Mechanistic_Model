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

krock <- read_csv("Data/data_input/lemuroid_chamber.csv")
fur_all<-read_csv("Data/data_input/fur_data_all_species.csv")

# Environment -------------------------------------------------------------

TAs <- krock$`Chamber temp`
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

TCs <- krock$`Possum temp`
#TC_MAX <- 40.8 # from Krockenberger et al 2012
TC_MAXs <- TCs
TC_INC <- 0

# Size and shape ----------------------------------------------------------
SHAPE_B <- 1.01
SHAPE_B_MAX <- 8
UNCURL <- 0.1
SHAPE <- 4
SAMODE <- 0 # try 2
PVEN <- 0.5 # this includes limbs in ventral surface; to obtain the pven without limbs see fur dataset
PCOND <- 0.07

# Fur properties ----------------------------------------------------------

DHAIRD <- 9.93E-06 # hair diameter, dorsal (m)
DHAIRV <- 9.43E-06 # hair diameter, ventral (m)
LHAIRD <- 0.02792025 # hair length, dorsal (m)
LHAIRV <- 0.0149395 # hair length, ventral (m)
ZFURD <- 0.016754175 # fur depth, dorsal (m)
ZFURV <- 0.004909167 # fur depth, ventral (m)  
RHOD <- 85992877.89 # borrowed from archeri
RHOV <- 51392321.53 # borrowed from archeri
REFLD <- 0.383 # borrowed from koala (NSW)
REFLV <- 0.523  # borrowed from koala (NSW)

# Physiological responses -------------------------------------------------

PCTWET <- 1.5
PCTWET_MAX <- 40
PCTWET_INC <- 0.25
PCTBAREVAP <- 3
Q10 <- 1.5 # 2 is default, but lower than calculated values

#Nat to check feasibility of making DELTAR always vary with predicted TLUNG

EXTREF <- 20 
PANT_INC <-  0.05
PANT_MAX <- 3 # before was 2
PANT_MULT <- 0 
AK1 <- 0.9
AK1_MAX<- 2.8
AK1_INC<- 0.1
ABSSB = 0.9 # solar absorptivity of substrate (fractional, 0-1)           # (CHECK THIS)


# CHECK THIS - Estimate metabolic rate based on allometric relationship

a <- (231 * 1000 / (24 * 60 * 60)) / AMASS ^ 0.737 # borrowed from archeri
QBASAL <- a * AMASS ^ 0.737

###!!!!#### here is the physiological element because it needs the TAs
#Also check setting this based on observed relationship in GGs
EXP_TEMP <- 0.76*TAs + 11.6 # at 10-25deg, exp = 0.76*TA - 11.6, 25-40 = 0.35x + 21.25
EXP_TEMP[TAs>=25] <- 0.35 * TAs[TAs>=25] + 21.35
DELTARs <- EXP_TEMP - TAs

################################
# 1.- RUN ENDO_R PROVIDING:    #
#     * CORE TEMPERATURE       #
#     * AMBIENT TEMPERATURE    #
#     * METABOLIC RATE         #
################################

# Run using mass of observed individuals + estimate metabolic rate based on allometric relationship

AMASSs<- (krock$`Body weight`)/1000


# Run end Or ---------------------------------------------------------------

endo.out_devel_run1 <- lapply(1:length(TAs), function(x) {
  endoR_devel_grtp(
    # ENVIRONMENT
    TA = TAs[x], VEL = VEL, RH = hum[x], # OPTION 1: DYNAMIC HUMIDITY
    #TA = TAs[x], VEL = VEL, RH = 10, # OPTION 2: STATIC HUMIDITY 
    ORIENT = 0, 
    FGDREF = 0.5, FSKREF = 0.5,
    # CORE TEMPERATURE
    #TC = TCs[x], TC_MAX = TCs[x], TC_INC = TC_INC, # OPTION 1: TC PER OBSERVATION
    TC = mean(krock$`Possum temp`, na.rm = T), TC_MAX = 40.8, TC_INC = 0.02, # OPTION 2: AVERAGE TC; TC_MAX = 40.8 (KROCKENBERGER ET AL 2012)
    # SIZE AND SHAPE
    AMASS = AMASSs[x], SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
    UNCURL = UNCURL, SAMODE = SAMODE, PVEN = PVEN,FATPCT = 10,
    PDIF = 1, ABSSB = ABSSB,
    # FUR PROPERTIES
    DHAIRV = DHAIRV, LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD,
    ZFURV = ZFURV, RHOD = RHOD, RHOV = RHOV, REFLD = REFLD, DHAIRD = DHAIRD,
    # PHYSIOLOGICAL RESPONSES
    PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, PCTWET_MAX = PCTWET_MAX,
    PCTBAREVAP = PCTBAREVAP,  AK1 = AK1, AK1_INC = AK1_INC, AK1_MAX = AK1_MAX,
    Q10 = Q10, QBASAL = QBASAL, DELTAR = DELTARs[x], PANT_INC = PANT_INC, # OPTION 1: Q10 PER OBSERVATION
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

obs_TCs <- krock$`Possum temp` # evaporative water loss
#obs_QGEN <- ((1000) * (krock$`RMR kJ/d`) / (24*60*60)) # metabolic rate
obs_H2O <- (krock$`Ewl (ml/g/min)`)*0.06*1000 # body temperature

# Comparison plos ---------------------------------------------------------

pred <- tibble(source = "NicheMapR",
               met_rate = QGEN,
               evap_water_loss = H2O,
               tc = TCs,
               mv = MV,
               air_t = TAs)
obs <- tibble(source = "Krock",
              met_rate = NA,
              evap_water_loss = obs_H2O,
              tc = obs_TCs,
              mv = NA,
              air_t = TAs)

comp <- rbind(pred, obs)



mt_comp <- comp %>% ggplot(aes(x = air_t, y = met_rate, col = source))+
  geom_point(size = 1, shape = 21, stroke = 2)+
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  labs(x = "Ambient temperature (°C)", y = "Metabolic rate (W)", col = "Source")+
  scale_color_manual(values = c("black", "red"))+
  theme_classic()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
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
#ggsave(filename = "1_2_1.png", path = "/Users/alejandrofp/Library/CloudStorage/OneDrive-JamesCookUniversity/PhD - projects/Ringtail - Mechanistic model - Wet Tropics/Ringtail_WT_Mechanistic_Model//Results/metabolic_chamber_results/custom_endoR_pven_no_full_thermoreg_seq")

