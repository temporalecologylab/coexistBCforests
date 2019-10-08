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
NMDS_RambspcPSPSFT2 <- read.csv("input/NMDS_RambspcPSPSFT2.csv", header=TRUE)
NMDS_RambspcPSPST2 <- read.csv("input/NMDS_RambspcPSPST2.csv", header=TRUE)
NMDS_RambspcPSPSPT2<- read.csv("input/NMDS_RambspcPSPSPT2.csv", header=TRUE)


colnames(NMDS_RambspcPSPSFT2)[1] <- "Species"
colnames(NMDS_RambspcPSPST2)[1] <- "Species"
colnames(NMDS_RambspcPSPSPT2)[1] <- "Species"

NMDS_RambspcPSP<- rbind(NMDS_RambspcPSPSFT2,NMDS_RambspcPSPST2,NMDS_RambspcPSPSPT2)

NMDS_RambspcPSP <- transform(NMDS_RambspcPSP, Light_quality = PFD.R/PFD.FR)

NMDS_RambspcPSP1 <- melt(NMDS_RambspcPSP[,c(1:2,5)], id.vars = c("Species", "Site"))

NMDS_RambspcPSP1$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", NMDS_RambspcPSP1$Species)

path <- unique(NMDS_RambspcPSP1$variable)


for (i in (path)){
  filename<- paste("output/LicormodelsPSP2",i,".csv", sep = "")
  li <- subset(NMDS_RambspcPSP1, variable == i)
  li <- na.omit(li)
  lm(value ~ Site + specieslatbi, data= li)
  s <- summary(lm(value ~ Site + specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
