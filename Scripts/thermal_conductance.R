###############################################################################
## NicheMapR - Comparison between observed and predicted thermal conductance ##
## Using with Green Ringtail Possum data                                     ##
###############################################################################

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(NicheMapR)
library(knitr)
library(plotrix)
library(patchwork)
library(geomtextpath)

# Working directory -------------------------------------------------------

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Ringtail - Mechanistic model - Wet Tropics/Data")


# Fur data ----------------------------------------------------------------

fur <- read_csv("data_input/fur_dataset.csv")

depth <- read_csv("data_input/fur_depth_grtp.csv")

depth <- depth %>% group_by(fur_id, side) %>% summarise(depth = mean(depth.m))

#####################################################
# In previous estimations, I used the fur depth averaged across sides (dorsal, ventral, and flank)
# to calculate thermal conductance. The result looked unrealistic for the ventral conductivity. It was too low.
# Lower than theoretically possible.
# A possible solution was to account for intra-variability across pelts. This was done by using the average depth (n = 6 measurements)
# for each individual pelt and side (ventral, dorsal, flank). This is a more logical approach than assuming a constant depth. Especially since thermal conductance is sensitive to depth.
# After account for this, the results look very similar. This is because the variability across pelts is not great, so we still get a very low value of thermal conductance (>0.025).
# Next, I considered human error. Did I induce bias on some of the measurements taken on the ventral part of the fur? This part is tricky to measure.
# This would be especially easy for the ventral area of the fur, because it is very short. A slight push with the caliper would mean double or triple the measure
# you would get. Thus, the sensitivity of manually measuring fur depth on the ventral part would be susceptible to human error.
# I consider this possibility by increasing the depth of the pelts with the lowest value by a error rate.
# I increased the error until the values of the ventral thermal conductance was closer, but below, the flank observed measurements (which would be the logical value they should get).
# I then compared this new observed measure to the effective thermal conductance simulated with NicheMapR, with the idea that the values should be higher than KEFF estimations.
# I would have to have induced an error across the measurements of 70-100% to have a value close to the expected reality (100% to be above the KEFF line).


depth <- depth %>% mutate(depth_error = case_when(
  side == "belly" & fur_id %in% c(2,3,4,5,6,7)~ depth + (depth*1), # It looks that fur 1 align with expectations, so only increased the depth in the other samples
  TRUE ~ depth
))

####################################################


###########################################
## Part I - check raw data for curvature ##
###########################################

# Fur insulation data -----------------------------------------------------

ins <- read_csv("data_input/fur_insulation_clean.csv") 
# raw data -> 37800 observations = 7 pelts * 3 sides (dorsal, ventral, flank) * 300 seconds of wind * 6 wind conditions (0,1,2,3,4,6m/s)
# Check for errors

raw_ins <- ins %>% ggplot(aes(time, `HFT (W)`, col = as.factor(`Fur ID`)))+
  geom_point()+
    labs(x = "Time (s)", col = "Fur ID", title = "Before")+
    facet_grid(ins$`Target wind speed (m/s)`~ins$`Fur region`, scales = "free")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
  

# It seems that there is a data entry **error** in flank*wind 3*fur 2

ins2 <- ins %>% filter(!(`Target wind speed (m/s)` == 3 & `Fur ID` == 2 & `Fur region` == "flank")) # here is the data fixed

correct_ins <- ins2 %>% ggplot(aes(time, `HFT (W)`, col = as.factor(`Fur ID`)))+
  geom_point()+
  labs(x = "Time (s)", col = "Fur ID", title = "After")+
  facet_grid(ins2$`Target wind speed (m/s)`~ins2$`Fur region`, scales = "free")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

raw_ins + correct_ins # compare data before and after removing the error

# Since there is not much variation across time (see plot above), we can assume that the observed thermal conductance is constant across time
# for the different wind treatments


######################################################
## Part II - calculate observed thermal conductance ##
######################################################

# Calculate all the difference in temperature between fur parts and ambient (2 cm above the tip)

ins2$delta_T_base_tip <- ins2$`Fur base temp (deg C)/Ts` - ins2$`Fur tip temp (deg C)` # delta T skin - T fur tip

ins2$delta_T_base_air <- ins2$`Fur base temp (deg C)/Ts` - ins2$`Air temp (deg C)/Ta` # delta T skin - T air

ins2$delta_T_tip_air <- ins2$`Fur tip temp (deg C)` - ins2$`Air temp (deg C)/Ta` # delta T tip - T air

ins2$side <- ins2$`Fur region`

ins2$fur_id <- ins2$`Fur ID`

# Add the constant depth based on the average across pelts. This can be used for comparison when the individual measurement was used

ins2 <- ins2 %>% mutate(constant_depth = case_when(
  side == "back" ~ fur[[4,6]],
  side == "belly" ~ fur[[10,6]],
  side == "flank" ~((fur[[5,6]] + fur[[6,6]])/2)
))

# Add fur thickness for the different fur areas

ins2 <- left_join(ins2, depth)

# Compare depth

ins2 %>% ggplot(aes(x = fur_id))+
  geom_point(aes(y = constant_depth), col = "black", size = 2)+
  geom_point(aes(y = depth_error),col = "red", size = 2)+
  geom_point(aes( y = depth), col = "blue", size = 2)+
  facet_wrap(~side)+
  annotate(geom = "text", col = "black", label = "Constant (mean)", x = 4, y = 0.025)+
  annotate(geom = "text", col = "blue", label = "Depth (actual measure)", x = 4, y = 0.0245)+
  annotate(geom = "text", col = "red", label = "Depth (100% error - double)", x = 4, y = 0.024)+
  labs(x = "Fur ID", y = "Depth (m)")+
  theme_classic()

# Solve for thermal conductance for the different delta T

# formula:  thermal conductance = thermal flux (HFT, W) * fur depth (m) / delta T (deg C)

# I calculated thermal conductivity for each combination of delta T

#//!!!\\ HERE YOU CAN SWITCH ON/OFF THE DIFFERENT OPTIONS FOR FUR DEPTH AMONG: 
#ERROR INDUCED (OPTION 1), CONSTANT DEPTH (OPTION 2), OR NO ERROR INDUCED (OPTION 3) #//!!|\\

## 1- Assuming error (using depth_error) and including intra-variability across furs

#ins2$thermal_cond_bt <- (-(ins2$`HFT (W)` * ins2$depth_error) / ins2$delta_T_base_tip)
#ins2$thermal_cond_ba <- (-(ins2$`HFT (W)` * ins2$depth_error) / ins2$delta_T_base_air)
#ins2$thermal_cond_ta <- (-(ins2$`HFT (W)` * ins2$depth_error) / ins2$delta_T_tip_air)

## 2- Assuming consistency across pelts

#ins2$thermal_cond_bt <- (-(ins2$`HFT (W)` * ins2$constant_depth) / ins2$delta_T_base_tip)
#ins2$thermal_cond_ba <- (-(ins2$`HFT (W)` * ins2$constant_depth) / ins2$delta_T_base_air)
#ins2$thermal_cond_ta <- (-(ins2$`HFT (W)` * ins2$constant_depth) / ins2$delta_T_tip_air)

## 3- Assume no-error but including intra-variability across furs

ins2$thermal_cond_bt <- (-(ins2$`HFT (W)` * ins2$depth) / ins2$delta_T_base_tip)
ins2$thermal_cond_ba <- (-(ins2$`HFT (W)` * ins2$depth) / ins2$delta_T_base_air)
ins2$thermal_cond_ta <- (-(ins2$`HFT (W)` * ins2$depth) / ins2$delta_T_tip_air)

# Summarise data per wind and fur area. This return the thermal conductance values at the species level, as it averages across the 7 pelts

# Here I am just creating new column with easier names for the grouping

ins2$T_air <- round(ins2$`Air temp (deg C)/Ta`, digits = 1) # I think this makes sense to avoid having more than 1000 temperature values
ins2$wind <- ins2$`Target wind speed (m/s)`

# Summarise data

summary_thermal_cond <- ins2 %>% group_by(T_air, wind, side, fur_id) %>% # group by air temperature, wind speed, and side of the fur (ventral, dorsal, flank)
  summarise(T_base = mean(`Fur base temp (deg C)/Ts`),
            T_tip = mean(`Fur tip temp (deg C)`),
            fur_depth = mean(depth_error),
            HFT = mean(`HFT (W)`),
            delta_T_ba = mean(delta_T_base_air),
            delta_T_bt = mean(delta_T_base_tip),
            delta_T_ta = mean(thermal_cond_ta),
            thermal_cond_ba_m = mean(thermal_cond_ba),
            thermal_cond_ta_m = mean(thermal_cond_ta),
            thermal_cond_bt_m = mean(thermal_cond_bt))

# Plot thermal conductance for individual pelts
summary_thermal_cond %>% 
  ggplot(aes(x = wind, y = thermal_cond_ba_m, col = as.factor(fur_id), group = as.factor(fur_id)))+
  geom_line(linewidth = 1)+
  labs(x = "Wind speed (m/s)", y = "Thermal conductance (W/m°C)", col = "Fur ID")+
  scale_color_viridis_d()+
  facet_wrap(~side)+
  theme_classic()

head(summary_thermal_cond, 10) # 488 combinations of T.air and wind speed
# NAs in the standard deviation is due to there is only one observation for that combination of Air temperature and Wind speed

# Boxplot to visualise the variability

summary_thermal_cond %>% ggplot(aes(x = wind, group = wind, fill = side))+
  #geom_line(aes(y = thermal_cond_ta), linetype = 3,  linewidth = 1)+
  #geom_line(aes(y = thermal_cond_ba_m), linetype = 2,  linewidth = 1)+
  #geom_line(aes(y = thermal_cond_bt_m), linetype = 1,  linewidth = 1)+
  geom_boxplot(aes(y = thermal_cond_ba_m))+
  scale_fill_viridis_d()+
  labs(x = "Air Temperature (°C)", y = "Thermal conductance (W/m°C)")+
  facet_wrap(~ side)+ # panels are different wind speeds
  theme_classic()

########################################################################
## Part III - Simulate the effective thermal conductance in NicheMapR ##
########################################################################

# Prepare data for loop

# I think using the summarised range of temperatures is justified instead of using the 37,500 raw values
# This is basically because the uncertainty is not very high (see summary table)


TAs <- summary_thermal_cond$T_air

TSKIN <- summary_thermal_cond$T_base # TSKIN = T_BASE

TFA <- summary_thermal_cond$T_tip # TFA = T_TIP


eff_thermal_cond_total <- tibble(side = NA, # empty dataframe to fill with the loop
                                 TA = NA, 
                                 TSKIN = NA, 
                                 TFA = NA,
                                 KEFF = NA, 
                                 KRAD = NA, 
                                 KFUR = NA)

# IRPROP subrutine --------------------------------------------------------

for(t in 1:length(TAs)){
  for(side in 1:3){ # 1 = mean, 2 = dorsal, 3 = ventral
    
    
    # environmental input
    TA <- TAs[t]  # air temperature, for calculation of conductivity of air (°C)
    
    # shape input
    PVEN <- fur[[22,6]]  # maximum fraction of surface area that is ventral (fractional, 0-1)
    
    # fur properties
    DHAIRD <- fur[[12,6]]  # hair diameter, dorsal (m)
    DHAIRV <- fur[[13,6]]  # hair diameter, ventral (m)
    LHAIRD <- fur[[1,6]]  # hair length, dorsal (m)  
    LHAIRV <- fur[[2,6]]  # hair length, ventral (m)  
    ZFURD <- fur[[4,6]]  # fur depth, dorsal (m)
    ZFURV <- fur[[10,6]]  # fur depth, ventral (m)
    ZFURCOMP <- ZFURV  # depth of compressed fur (for conduction) (m)
    RHOD <- fur[[16,6]]  # hair density, dorsal (1/m2) 
    RHOV <- fur[[19,6]]  # hair density, ventral (1/m2)
    REFLD <- 0.301  # fur reflectivity dorsal (fractional, 0-1) 
    REFLV <- 0.301  # fur reflectivity ventral (fractional, 0-1)
    KHAIR <- 0.209  # hair thermal conductivity (W/m°C)
    
    # call the subroutine
    IRPROP.out <- IRPROP(TA, DHAIRD, DHAIRV, LHAIRD, LHAIRV, ZFURD,
                         ZFURV, RHOD, RHOV, REFLD, REFLV, ZFURCOMP, PVEN, KHAIR)
    
    # output
    KEFARA <- IRPROP.out[1:3]  # effective thermal conductivity of fur array, mean, dorsal, ventral (W/mK)
    BETARA <- IRPROP.out[4:6]  # term involved in computing optical thickness (1/m)
    #B1ARA <- IRPROP.out[7:9]  # optical thickness array, mean, dorsal, ventral (-)
    #DHAR <- IRPROP.out[10:12]  # fur diameter array, mean, dorsal, ventral (m)
    #LHAR <- IRPROP.out[13:15]  # fur length array, mean, dorsal, ventral (m)
    #RHOAR <- IRPROP.out[16:18]  # fur density array, mean, dorsal, ventral (fibers/m2)  
    #ZZFUR <- IRPROP.out[19:21]  # fur depth array, mean, dorsal, ventral (m)  
    #REFLFR <- IRPROP.out[22:24]  # fur reflectivity array, mean, dorsal, ventral (fractional, 0-1)
    #FURTST <- IRPROP.out[25]  # test of presence of fur (length x diameter x density x depth) (-)
    #KFURCMPRS <- IRPROP.out[26]  # effective thermal conductivity of compressed ventral fur (W/mK)
    
    IRPROP.lab <- c("KEFARA mean", "KEFARA dorsal", "KEFARA ventral",
                    "BETARA mean", "BETARA dorsal", "BETARA ventral"
                    #, "B1ARA mean",
                    #"B1ARA dorsal", "B1ARA ventral", "DHAR mean", "DHAR dorsal",
                    #"DHAR ventral", "LHAR mean", "LHAR dorsal", "LHAR ventral",
                    #"RHOAR mean", "RHOAR dorsal", "RHOAR ventral", "ZZFUR mean",
                    #"ZZFUR dorsal", "ZZFUR ventral", "REFLFR mean", "REFLFR dorsal",
                    #"REFLFR ventral", "FURTST", "KFURCMPRS"
                    )
    
    #kable(cbind(IRPROP.lab, IRPROP.out[1:26]))
    
    
# Calculate KRAD for each measurement
    
    SIG	<- 5.6697E-08
    
    XR <- 1
    
    #TSKIN = Tbase and TFA = Tfur-tip
    
    TRAPPX = (TSKIN[t]*(1 - XR))+(TFA[t]*XR)
    
    KRAD = (16.0*SIG*(TRAPPX+273.15)^3)/(3*BETARA[side])
    
    KEFF <- KEFARA[side]
    
    KFUR = KEFF + KRAD
    
    ##Write out KEFF, KRAD & KFUR for each fur
    
    vec <- c(side, TA, TSKIN[t], TFA[t] ,KEFF, KRAD, KFUR)
    
    eff_thermal_cond_total <- rbind(eff_thermal_cond_total, vec)
    
  }
  
}

eff_thermal_cond_total <- eff_thermal_cond_total[-1,]


#############################################
## Part IV - compare observed vs predicted ##
#############################################

summary_thermal_cond_Tair <- ins2 %>% group_by(T_air, side) %>% # group by air temperature, wind speed, and side of the fur (ventral, dorsal, flank)
  summarise(T_base = mean(`Fur base temp (deg C)/Ts`),
            T_tip = mean(`Fur tip temp (deg C)`),
            fur_depth = mean(depth),
            HFT = mean(`HFT (W)`),
            delta_T_ba = mean(delta_T_base_air),
            delta_T_bt = mean(delta_T_base_tip),
            delta_T_ta = mean(thermal_cond_ta),
            thermal_cond_ba_m = mean(thermal_cond_ba),
            sd_thermal_cond_ba = sd(thermal_cond_ba, na.rm = T),
            thermal_cond_ta_m = mean(thermal_cond_ta),
            sd_thermal_cond_ta = sd(thermal_cond_ta),
            thermal_cond_bt_m = mean(thermal_cond_bt),
            sd_thermal_cond_bt = sd(thermal_cond_bt))



obs <- summary_thermal_cond_Tair

obs <- obs %>% filter(side != "flank") # filter out flank side, not yet estimated in the loop above

pred <- eff_thermal_cond_total

pred <- pred %>% filter(side != 1) # filter out mean conductance for comparison

pred$T_air <- pred$TA

pred <- pred[,-2]


pred <- pred %>% mutate(side = case_when( # match fur side from datasets to join them
  side == 2 ~ "back",
  side == 3 ~ "belly"
 ))

comp <- left_join(pred, obs)


comp %>% 
  ggplot(aes(x = T_air))+
  geom_textline(aes(y = KEFF), col = "red", label = "KEFF", hjust = 1)+
  geom_textline(aes(y = KRAD), col = "blue", label = "KRADU", hjust = 1.025)+
  geom_textline(aes(y = KFUR), col = "green", label = "KFUR", hjust = 1.025)+
  geom_textline(aes(y = thermal_cond_ba_m), label = "BA", hjust = 1)+
  labs(x = "Air Temperature (°C)", y = "Thermal conductance (W/m°C)")+
  #geom_textline(aes(y = thermal_cond_bt_m), label = "BT", hjust = 1)+
  facet_grid(~ side, scales = "free")+
  theme_classic()

