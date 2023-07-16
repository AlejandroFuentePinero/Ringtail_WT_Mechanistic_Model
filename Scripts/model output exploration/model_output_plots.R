library(tidyverse)
library(zoo)


# TC ----------------------------------------------------------------------

species <- "archeri"

model <- "model_1_possum"

dataset <- "treg"

variable_name <- "TC"

threshold <- 36.8 # one degree above TC


load(paste0("model_output/",species,"_output_",model,"_",dataset,"_", variable_name, ".RData"))

tc_check <- which(data$TC == 40.8)
check_pant <- which(data$PANT == 3)

die <- tc_check[tc_check %in% check_pant]

data <- out$full_data

data[check_pant,] %>% ggplot(aes(dates, TC))+
  geom_point()+
  facet_wrap(~site)

stress <- data %>%
  group_by(site) %>% 
  mutate(threshold = threshold,
         stress = case_when(
         !!sym(variable_name) > threshold ~ (!!sym(variable_name) - threshold), # amount of stress
    TRUE ~ 0 # set to zero no stress
  )) %>% 
  mutate(stress_3 = rollsumr(stress, k = 24*3, fill = NA, align = "right")) %>%  # 3 days
  ungroup() 


# all pattern

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = dates))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()


# monthly

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = month(dates)))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()



# 2 years

stress %>% filter(site %in% c("AU4A", "AU8A"),
                  year %in% c(1997, 1998)) %>% ggplot(aes(x = dates))+
  geom_line(aes(y = stress_3), size = 1)+
  facet_grid(site~year, scales = "free")+
  theme_bw()


# rain

ylim.prim <- c(min(stress$stress_3, na.rm = T), max(stress$stress_3, na.rm = T))  
ylim.sec <- c(min(rain$rain, na.rm = T), max(rain$rain, na.rm = T))    


b <- diff(ylim.prim)/diff(ylim.sec) * 0.02
a <- ylim.prim[1] - (b*ylim.sec[1]) # there was a bug here

rain <- out$rain_df

rain_inf <- rain %>% filter(site %in% c("AU8A"),
                   year %in% c(1997))

stress_inf <- stress %>% filter(site %in% c("AU8A"),
                     year %in% c(1997))

ggplot(data = stress_inf, aes(x = dates, y = stress_3))+
  geom_col(data = rain_inf %>% filter(rain > 5), aes(x = date, y = a+b*rain))+
  geom_line(size = 1, col = "red")+
  scale_y_continuous("Stress", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "rain (mm)"))+
  theme_bw()
  

# Pant --------------------------------------------------------------------

species <- "archeri"

model <- "model_1_possum"

dataset <- "treg"

variable_name <- "PANT"

threshold <- 2 


load(paste0("model_output/",species,"_output_",model,"_",dataset,"_", variable_name, ".RData"))

data <- out$full_data


check_pant <- which(data$PANT == 3)

stress <- data %>%
  group_by(site) %>% 
  mutate(threshold = threshold,
         stress = case_when(
           !!sym(variable_name) > threshold ~ (!!sym(variable_name) - threshold), # amount of stress
           TRUE ~ 0 # set to zero no stress
         )) %>% 
  mutate(stress_3 = rollsumr(stress, k = 24*3, fill = NA, align = "right")) %>%  # 3 days
  ungroup() 


# all pattern

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = dates))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()


# monthly

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = month(dates)))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()



# 2 years

stress %>% filter(site %in% c("AU4A", "AU8A"),
                  year %in% c(1997, 1998)) %>% ggplot(aes(x = dates))+
  geom_line(aes(y = stress_3), size = 1)+
  facet_grid(site~year, scales = "free")+
  theme_bw()


# rain

ylim.prim <- c(min(stress$stress_3, na.rm = T), max(stress$stress_3, na.rm = T))  
ylim.sec <- c(min(rain$rain, na.rm = T), max(rain$rain, na.rm = T))    


b <- diff(ylim.prim)/diff(ylim.sec) * 0.8
a <- ylim.prim[1] - (b*ylim.sec[1]) # there was a bug here

rain <- out$rain_df

rain_inf <- rain %>% filter(site %in% c("AU4A"),
                            year %in% c(1996))

stress_inf <- stress %>% filter(site %in% c("AU4A"),
                                year %in% c(1996))

ggplot(data = stress_inf, aes(x = dates, y = stress_3))+
  geom_col(data = rain_inf %>% filter(rain > 5), aes(x = date, y = a+b*rain))+
  geom_line(size = 1, col = "red")+
  scale_y_continuous("Stress", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "rain (mm)"))+
  theme_bw()


# water -------------------------------------------------------------------


species <- "archeri"

model <- "model_1_possum"

dataset <- "masbal"

variable_name <- "H2O"

# calculate threshold as 1% body weight loss to water

weight <- 1170

loss_5pc_day <- weight * 0.01

grams_per_hour <- loss_5pc_day / 24

threshold <- grams_per_hour



load(paste0("model_output/",species,"_output_",model,"_",dataset,"_", variable_name, ".RData"))

data <- out$full_data

stress <- data %>%
  group_by(site) %>% 
  mutate(threshold = threshold,
         stress = case_when(
           !!sym(variable_name) > threshold ~ (!!sym(variable_name) - threshold), # amount of stress
           TRUE ~ 0 # set to zero no stress
         )) %>% 
  mutate(stress_3 = rollsumr(stress, k = 24*3, fill = NA, align = "right")) %>%  # 3 days
  ungroup() 


# all pattern

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = dates))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()


# monthly

stress %>% filter(site %in% c("AU4A", "AU8A")) %>% ggplot(aes(x = month(dates)))+
  geom_point(aes(y = stress_3))+
  facet_wrap(~site)+
  theme_bw()



# 2 years

stress %>% filter(site %in% c("AU4A", "AU8A"),
                  year %in% c(1997, 1998)) %>% ggplot(aes(x = dates))+
  geom_line(aes(y = stress_3), size = 1)+
  facet_grid(site~year, scales = "free")+
  theme_bw()


# rain

ylim.prim <- c(min(stress$stress_3, na.rm = T), max(stress$stress_3, na.rm = T))  
ylim.sec <- c(min(rain$rain, na.rm = T), max(rain$rain, na.rm = T))    


b <- diff(ylim.prim)/diff(ylim.sec) * 0.8
a <- ylim.prim[1] - (b*ylim.sec[1]) # there was a bug here

rain <- out$rain_df

rain_inf <- rain %>% filter(site %in% c("AU4A"),
                            year %in% c(1996))

stress_inf <- stress %>% filter(site %in% c("AU4A"),
                                year %in% c(1996))

ggplot(data = stress_inf, aes(x = dates, y = stress_3))+
  geom_col(data = rain_inf %>% filter(rain > 5), aes(x = date, y = a+b*rain))+
  geom_line(size = 1, col = "red")+
  scale_y_continuous("Stress", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "rain (mm)"))+
  theme_bw()


# energy ------------------------------------------------------------------



species <- "archeri"

model <- "model_1_possum"

dataset <- "enbal"

variable_name <- "energy"

load(paste0("model_output/",species,"_output_",model,"_",dataset,"_", variable_name, ".RData"))


data <- out$full_data


summary(data$QSOL)



data$day <- yday(data$dates)
micro <- out$microclimate_df

variable_name <- "QGEN"
qbasal <- (70 * 1.17 ^ 0.75) * (4.185 / (24 * 3.6))


data[die,] %>% 
  ggplot(aes(dates, QGEN))+
  geom_point()+
  theme_bw()


data %>% filter(s)




summary(data$QGEN)

# PCTWET ------------------------------------------------------------------


species <- "archeri"

model <- "model_1_possum"

dataset <- "treg"

variable_name <- "TC"

load(paste0("model_output/",species,"_output_",model,"_",dataset,"_", variable_name, ".RData"))

data <- out$full_data

summary(data$PCTWET)

data %>% ggplot(aes(dates, PCTWET))+
  geom_point()+
  facet_wrap(~site)
