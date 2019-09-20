#Licormodels for all pacific spirit park sites
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")
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
NMDS_RambspcPSPSFT <- read.csv("input/NMDS_RambspcPSPSFT.csv", header=TRUE)
NMDS_RambspcPSPST <- read.csv("input/NMDS_RambspcPSPST.csv", header=TRUE)
#Insert site column in excel
NMDS_RambspcPSPSPT<- read.csv("input/NMDS_RambspcPSPSPT.csv", header=TRUE)


colnames(NMDS_RambspcPSPSFT)[1] <- "Species"
colnames(NMDS_RambspcPSPST)[1] <- "Species"
colnames(NMDS_RambspcPSPSPT)[1] <- "Species"

NMDS_RambspcPSP<- rbind(NMDS_RambspcPSPSFT,NMDS_RambspcPSPST,NMDS_RambspcPSPSPT)

NMDS_RambspcPSP1 <- melt(NMDS_RambspcPSP, id.vars = c("Species", "Site"))

NMDS_RambspcPSP1$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", NMDS_RambspcPSP1$Species)

path <- unique(NMDS_RambspcPSP1$variable)


for (i in (path)){
  filename<- paste("output/LicormodelsPSP",i,".csv", sep = "")
  li <- subset(NMDS_RambspcPSP1, variable == i)
  li <- na.omit(li)
  lm(value ~ Site + specieslatbi, data= li)
  s <- summary(lm(value ~ Site + specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
