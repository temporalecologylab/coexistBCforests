#preliminary Data Analysis for Field Data
#started by Darwin on August 24th 2020

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

#load packages
library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(lme4)


#load data
SEEDLINGDATA <- read_excel("input/SEEDLINGDATA.xlsx")
FIELDDATA <- read_excel("input/FIELDDATA.xlsx")


#Cleaning data

#seperates species names from species number
FIELDDATA <- FIELDDATA %>% mutate(V2 = str_extract(FIELDDATA$Tree_ID, "[0-9]+"),
              V3 = str_extract(FIELDDATA$Tree_ID, "[aA-zZ]+"))

#renames columns in species names and species epithet
colnames(FIELDDATA)[5] <- "tree_number"
colnames(FIELDDATA)[6] <- "tree_species"

#Replaces all misspelt tree species ID
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "AIBLAS", "ABILAS")
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "PICHB", "PICHYB")
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "PICHHYB", "PICHYB")
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "PICHY", "PICHYB")
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "PICON", "PINCON")
FIELDDATA<- replace(FIELDDATA, FIELDDATA== "AIBLAS", "ABILAS")

#Aggregates DBH data by tree species and plot
aggdata<- aggregate( FIELDDATA$`DBH (CM)` ~ FIELDDATA$Plot + FIELDDATA$tree_species, FIELDDATA, mean)

#Renames columns so that aggregate data and FIELDDATA match
colnames(aggdata)[1] <- "Plot"
colnames(aggdata)[2] <- "Germinant_ID"

#merges data by plot number and species epitet
joined_Data <- merge(SEEDLINGDATA, aggdata, by= c("Plot", "Germinant_ID"))

colnames(joined_Data)[6] <- "conspecific.distance"
colnames(joined_Data)[8] <- "stand.density"



lm(Height ~ conspecific.distance + stand.density, data=joined_Data)
summary(lmer(Height ~ conspecific.distance + (1|stand.density), data=joined_Data))

anova(lmer(Height ~ conspecific.distance + (1|stand.density), data=joined_Data))

plot(Height  ~ stand.density, data=joined_Data)
lm(Height  ~ stand.density, data=joined_Data)
summary(lm(Height  ~ stand.density, data=joined_Data))

plot(Height  ~ conspecific.distance + stand.density, data=joined_Data)
lm(Height  ~ conspecific.distance + stand.density, data=joined_Data)
summary(lm(Height  ~ conspecific.distance + stand.density, data=joined_Data))

#plots

#Plots raw data of seedling height by distance to closest conspecific 
ggplot(data=SEEDLINGDATA, aes(x=SEEDLINGDATA$`Distance to Conspecific`, y=Height)) +
         geom_point(aes(color= Germinant_ID)) + geom_smooth(method='lm') 

ggplot(data=joined_Data, aes(x=conspecific.distance, y=Height)) + geom_point(aes(color= stand.density)) +
  geom_smooth(method='lm') 
