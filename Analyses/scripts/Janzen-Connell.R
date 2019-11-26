# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

library(readxl)
library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)

Janzen_Connell_ <- read_excel("~/Documents/GitHub/Coexistence-in-BC-Forests/Data/Janzen-Connell /Janzen-Connell .xlsx")

######################################################################################
#plotting all data at once
ggplot(Janzen_Connell_, )

ggplot(Janzen_Connell_, aes(x = health_scale, y =Closest_adult, color= Species )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

ggplot(Janzen_Connell_, aes(x = health_scale, y = Height, color= Species )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

a <- aov(Closest_adult~health_scale, data= Janzen_Connell_)

swaelee <- lmer(health_scale ~ Closest_adult + (1 | Site) + (1 | Height), data= Janzen_Connell_)
summary (swaelee)

anova(swaelee )

################################################################################
#Plotting species individually

path <- unique(Janzen_Connell_$Species)

for (i in 1:length(path)){
  kiasmos <- subset(Janzen_Connell_, Species == path[i]) #selects variables that contain the species
  kiasmos<- na.omit(kiasmos) #omits NAs
  kiasmos <- as.data.frame(kiasmos)
  plotname <- paste("output/JC",path[i],".pdf", sep="") #plots each Species seperately 
  pdf(file= plotname)
  #without print the PDF do not save!
  print(ggplot(kiasmos, aes(x = health_scale, y =Closest_adult)) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1))) 
  dev.off()
}
