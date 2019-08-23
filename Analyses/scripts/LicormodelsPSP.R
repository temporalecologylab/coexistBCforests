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

NMDS_RambspcPSPST <- read.csv("input/NMDS_RambspcPSPST.csv", header=FALSE)

colnames(NMDS_RambspcPSPST) <- NMDS_RambspcPSPST[1,]
NMDS_RambspcPSPST <- NMDS_RambspcPSPST[-1,]

colnames(NMDS_RambspcPSPST)[1] <- "Species"


NMDS_RambspcPSPST$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", NMDS_RambspcPSPST$Species)

nmds1 <- melt(NMDS_RambspcPSPST, id.vars = c("Species", "specieslatbi"))

path <- unique(nmds1$variable)



for (i in (unique(nmds1$variable))){
  filename<- paste("output/Licormodelspsp",i,".csv", sep = "")
  li <- subset(nmds1, variable == i)
  lm(value ~ specieslatbi, data= li)
  s <- summary(lm(value ~ specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
