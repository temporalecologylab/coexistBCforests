rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/LI-COR Data (Manning Park)")

library(vegan)
library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)

LiCordatafinal <- read_csv("LiCordatafinal.csv")

#fixeserrors in species' names
LiCordatafinal$Species <- gsub(x= LiCordatafinal$Species, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$Species<- gsub(x= LiCordatafinal$Species, pattern = "TSHUET", replacement = "TSUHET")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "TSHUET", replacement = "TSUHET")


#subsetting dataset for adult trees only!
LiCordata_adult <- LiCordatafinal[LiCordatafinal$seedlingoradult == 'a',]
LiCordata_adult <- na.omit(LiCordata_adult)
LiCordata_adult <- LiCordata_adult[,c(1:2,418:424,448,451)]
LiCordata_adult[, c(3:9)] <- sapply(LiCordata_adult[, c(3:9)], as.numeric)

#makes rowsname species names 
rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)


#created dataset for one site and all PFD readings
k <- setDT(LiCordata_adult, row.names(LiCordata_adult))
k <- dcast(k, k$BETPAP01 ~ k$Site,
          value.var = c("PFD-B", "PFD-FR", "PFD-G", "PFD-R", "PFD-UV"))

#creates rownames which are species codes
str(k)
k <- as.data.frame(k)
rownames(k) <- make.names(k$k)


#selects only one site
k1 <- select(k, matches("_CF1"))

#omits any NAs
k1 <- na.omit(k1)


#creates matrix
community_matrix <- as.matrix(k1)


#runs NMDS
example_NMDS=metaMDS(community_matrix, k=3, wascores = TRUE)

#creates stressplot to understand goodness of fit for 2d vs 3D
stressplot(example_NMDS)
plot(example_NMDS)

#creates ordination plot
ordiplot(example_NMDS,type="t")

plot(example_NMDS, main="Species Ordination", type="t", display="species")

plot(example_NMDS, main="Site Ordination", type="t", display="sites", air=1)

orditorp(example_NMDS,display="species",col="red",air=1)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.5)
summary(example_NMDS)
example_NMDS$species

d<- vegdist(community_matrix, binary = "TRUE", method = "bray")


LiCordatafinal <- read_csv("LiCordatafinal.csv")

#fixeserrors in species' names
LiCordatafinal$Species <- gsub(x= LiCordatafinal$Species, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$Species<- gsub(x= LiCordatafinal$Species, pattern = "TSHUET", replacement = "TSUHET")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "TSHUET", replacement = "TSUHET")


#subsetting dataset for adult trees only!
LiCordata_adult <- LiCordatafinal[LiCordatafinal$seedlingoradult == 'a',]
LiCordata_adult <- na.omit(LiCordata_adult)
LiCordata_adult <- LiCordata_adult[,c(1:2,426:440,448,451)]
LiCordata_adult[, c(3:14)] <- sapply(LiCordata_adult[, c(3:14)], as.numeric)

#makes rowsname species names 
rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)

#created dataset for all "R" reading
j <- setDT(LiCordata_adult, row.names(LiCordata_adult))
j <- dcast(j, j$BETPAP01 ~ j$Site,
           value.var = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
                         "R11", "R12", "R13", "R14", "R15"))

j<- as.data.frame(j)
rownames(j) <- make.names(j$j)


#selects only one site
j <- select(j, matches("_CF1"))

#omits any NAs
j <- na.omit(j)

#creates matrix
community_matrix <- as.matrix(j)


#runs NMDS
example_NMDS=metaMDS(community_matrix, k=3, wascores = TRUE)

#creates stressplot to understand goodness of fit for 2d vs 3D
stressplot(example_NMDS)
plot(example_NMDS)

#creates ordination plot
ordiplot(example_NMDS,type="t")

#creates ordination plot based on readings
plot(example_NMDS, main="Species Ordination", type="t", display="species")

#creates ordination plot based on species
plot(example_NMDS, main="Site Ordination", type="t", display="sites", air=1)
