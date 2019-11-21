# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Data/Janzen-Connell\ /")
library(readxl)

Janzen_Connell_ <- read_excel("~/Documents/GitHub/Coexistence-in-BC-Forests/Data/Janzen-Connell\ /Janzen-Connell\ .xlsx  ")

ggplot(Janzen_Connell_, )

ggplot(Janzen_Connell_, aes(x = health_scale, y =Closest_adult, color= Species )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

ggplot(Janzen_Connell_, aes(x = health_scale, y = Height, color= Species )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

a <- aov(Closest_adult~health_scale, data= Janzen_Connell_)

swaelee <- lmer(health_scale ~ Closest_adult + (1 | Site) + (1 | Height), data= Janzen_Connell_)
summary (swaelee)

anova(swaelee )
