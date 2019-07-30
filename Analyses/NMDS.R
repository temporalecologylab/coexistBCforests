rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/LI-COR Data (Manning Park)")

library(vegan)
library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)

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

rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)

community_matrix=matrix(
  +     LiCordata_adult$`PFD-B`),nrow=645,
  +     dimnames=list(paste(LiCordata_adult$Species),paste(LiCordata_adult$Site))


community_matrix <- (sapply(LiCordata_adult[3:9], as.numeric))
community_matrix<- cbind(LiCordata_adult[1],community_matrix )
community_matrix<- cbind(LiCordata_adult[2],community_matrix )
community_matrix<- cbind(LiCordata_adult[10],community_matrix )

k<- dcast(LiCordata_adult, row.names(LiCordata_adult) ~ LiCordata_adult$Site, value.var = 'PFD-B')
k <- `row.names<-`(k, k$`row.names(LiCordata_adult)`)
k <- k[-1]
k1 <- k[,c(1)]

k1 <- select(k, 'CF1')
k1 <- na.omit(k1)
community_matrix <- as.matrix(k1)

community_matrix[is.na(community_matrix)] <- 0

example_NMDS=metaMDS(community_matrix, k=2, trymax = 100)
stressplot(example_NMDS)
plot(example_NMDS)

ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)


rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)

str(k)
