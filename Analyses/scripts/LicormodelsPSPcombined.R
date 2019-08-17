setwd("~/Documents/Ph.D/LI-COR Data (pacific spirit park)")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)

#Import datasets
NMDS_RambspcPSPSFT <- read.csv("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/input/NMDS_RambspcPSPSFT.csv", header=TRUE)
NMDS_RambspcPSPST <- read.csv("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/input/NMDS_RambspcPSPST.csv", header=TRUE)


colnames(NMDS_RambspcPSPSFT)[1] <- "Species"
colnames(NMDS_RambspcPSPST)[1] <- "Species"



NMDS_RambspcPSP<- rbind(NMDS_RambspcPSPSFT,NMDS_RambspcPSPST)

NMDS_RambspcPSP1 <- melt(NMDS_RambspcPSP, id.vars = c("Species", "Site"))

NMDS_RambspcPSP1$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", NMDS_RambspcPSP1$Species)

path <- unique(NMDS_RambspcPSP1$variable)

for (i in (unique(NMDS_RambspcPSP1$variable))){
  filename<- paste("LicormodelsPSP",i,".csv", sep = "")
  li <- subset(NMDS_RambspcPSP1, variable == i)
  li <- na.omit(li)
  lm(value ~ Site + specieslatbi, data= li)
  s <- summary(lm(value ~ Site + specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
