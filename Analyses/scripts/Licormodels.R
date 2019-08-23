###################################################
#Models attempting to code dataset to run linear models
###################################################
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/input/NMDS")

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)


nmds <-
  list.files(pattern = "*.csv") %>%
  map_df(~read.csv(.))


nmds$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", nmds$X)

nmds1 <- melt(nmds, id.vars = c("X", "specieslatbi"))
nmds1 <- na.omit(nmds1)
nmds1$site <- (sub("^[^_]*_", "", nmds1$variable))
nmds1$variable <- gsub("_.*", "",nmds1$variable)


path <- unique(nmds1$variable)

for (i in (unique(nmds1$variable))){
  filename<- paste("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/output/Licormodels",i,".csv", sep = "")
  li <- subset(nmds1, variable == i)
  li <- na.omit(li)
  lm(value ~ site + specieslatbi, data= li)
  s <- summary(lm(value ~ site + specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}

#############################
#FR readings 
#############################

setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/input/Rreadings")

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)


nmds <-
  list.files(pattern = "*.csv") %>%
  map_df(~read.csv(.))


nmds$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", nmds$X)

nmds1 <- melt(nmds, id.vars = c("X", "specieslatbi"))
nmds1 <- na.omit(nmds1)
nmds1$site <- (sub("^[^_]*_", "", nmds1$variable))
nmds1$variable <- gsub("_.*", "",nmds1$variable)


path <- unique(nmds1$variable)

for (i in (unique(nmds1$variable))){
  filename<- paste("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/output/Licormodels",i,".csv", sep = "")
  li <- subset(nmds1, variable == i)
  li <- na.omit(li)
  lm(value ~ site + specieslatbi, data= li)
  s <- summary(lm(value ~ site + specieslatbi, data= li))
  s <- as.data.frame(s$coefficients)
  write.csv(s, filename)
}
