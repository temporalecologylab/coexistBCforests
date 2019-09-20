###################################################
#Models attempting to code dataset to run linear models
###################################################
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

NMDS_RambspcPSPSPT <- read.csv("input/NMDS_RambspcPSPSPT.csv", header=FALSE)

colnames(NMDS_RambspcPSPSPT) <- NMDS_RambspcPSPSPT[1,]
NMDS_RambspcPSPSPT <- NMDS_RambspcPSPSPT[-1,]

colnames(NMDS_RambspcPSPSPT)[1] <- "Species"


NMDS_RambspcPSPSPT$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", NMDS_RambspcPSPSPT$Species)

nmds1 <- melt(NMDS_RambspcPSPSPT, id.vars = c("Species", "specieslatbi"))

path <- unique(nmds1$variable)


for (i in (unique(nmds1$variable))){
  filename<- paste("output/LicormodelsPSPSPT",i,".csv", sep = "")
  li <- subset(nmds1, variable == i)
  lm(value ~ specieslatbi, data= li)
  s <- summary(lm(value ~ specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
