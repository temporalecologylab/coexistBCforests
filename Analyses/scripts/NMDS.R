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
library(dplyr)

###############################################
#Comparing species within sites
##############################################

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
k <- k[,-1]

path <- unique(LiCordata_adult$Site)

for (i in 1:length(path)){
  k1 <- select(k, contains(path[i])) #selects variables that contain the sites 
  k1 <- na.omit(k1) #omits NAs
  community_matrix <- as.matrix(k1) #creates matrix
  example_NMDS=metaMDS(community_matrix, k=3, wascores = TRUE) #runs NMDS
  plotname <- paste("NMDS",path[i],".pdf", sep="") #plotseach site seperately 
  pdf(file= plotname)
  stressplot(example_NMDS)
  plot(example_NMDS)
  plot(example_NMDS, main="Species Ordination", type="t", display="species")
  plot(example_NMDS)
  orditorp(example_NMDS,display="sites",cex= 0.5,air= 0.3)#labels points as best as we can
  dev.off()
}

###########################################
#Comparing sites across species 
##########################################

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

#created dataset for all "R" reading
j <- setDT(LiCordata_adult, row.names(LiCordata_adult))
j <- dcast(j, j$Site+j$BETPAP01  ~ j$specieslatbi,
                value.var = c("PFD-B", "PFD-FR", "PFD-G", "PFD-R", "PFD-UV"))

j <- j[,-2]

#get path for unique species but removes POPTRE & BETPAP as they appear only in one site 
path <- unique(LiCordata_adult$specieslatbi)
path <- path[c(-1, -5)]


for (i in 1:length(path)){
  j1 <- select(j,j, contains(path[i]))# selects species column and seperates readings by species
  j1 <- na.omit(j1)
  j2<- j1 %>% group_by(j) %>% summarise_all(funs(mean))#if there are mutliple readings then take the mean
  rownames(j2) <- make.names(j2$j) #makes sitenames as row names
  j2<- as.data.frame(j2)
  j2 <- j2[,-1]
  community_matrix <- as.matrix(j2) #creates matrix
  example_NMDS=metaMDS(community_matrix, wascores = TRUE) #runs NMDS
  plotname <- paste("NMDS",path[i],".pdf", sep="") #plots results!
  pdf(file= plotname)
  stressplot(example_NMDS)
  plot(example_NMDS)
  plot(example_NMDS, main="Species Ordination", type="t", display="species")
  plot(example_NMDS)
  orditorp(example_NMDS,display="sites",cex= 1,air= 0.5)
  dev.off()
}
 
#########################################################
#ambeint readings
########################################################

LiCordatafinal <- read_csv("LiCordatafinal.csv")

LiCordata_adult <- LiCordatafinal[LiCordatafinal$Species == 'AMBIENT',]
LiCordata_adult <- LiCordata_adult[,c(1:2,418:424,448,451)]
LiCordata_adult[, c(3:9)] <- sapply(LiCordata_adult[, c(3:9)], as.numeric)

#makes rowsname species names 
rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)

#created dataset for all "R" reading
l <- setDT(LiCordata_adult, row.names(LiCordata_adult))
l <- dcast(l, l$AMBIENT  ~ l$Site,
           value.var = c("PFD-B", "PFD-FR", "PFD-G", "PFD-R", "PFD-UV"))

#creates rownames which are species codes
str(l)
l <- as.data.frame(l)
rownames(l) <- make.names(l$l)
l <- l[,-1]

path <- unique(LiCordata_adult$Site)

for (i in 1:length(path)){
  l1 <- select(l, contains(path[i])) #selects variables that contain the sites 
  l1 <- na.omit(l1) #omits NAs
  community_matrix <- as.matrix(l1) #creates matrix
  example_NMDS=metaMDS(community_matrix, k=2, wascores = TRUE) #runs NMDS
  plotname <- paste("AMBIENTNMDS",path[i],".pdf", sep="") #plotseach site seperately 
  pdf(file= plotname)
  stressplot(example_NMDS)
  plot(example_NMDS)
  plot(example_NMDS, main="Species Ordination", type="t", display="species")
  plot(example_NMDS)
  orditorp(example_NMDS,display="sites",cex= 0.5,air= 0.3)#labels points as best as we can
  dev.off()
}

#########################################################
#species-ambeint readings
########################################################

LiCordatafinal <- read_csv("LiCordatafinal.csv")

#fixeserrors in species' names
LiCordatafinal$Species <- gsub(x= LiCordatafinal$Species, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$Species<- gsub(x= LiCordatafinal$Species, pattern = "TSHUET", replacement = "TSUHET")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "lONSOF", replacement = "LONSOF")
LiCordatafinal$specieslatbi<- gsub(x= LiCordatafinal$specieslatbi, pattern = "TSHUET", replacement = "TSUHET")

LiCordata_adult <- LiCordatafinal[LiCordatafinal$seedlingoradult == 'a',]

LiCordata_adult1 <- LiCordatafinal[LiCordatafinal$Species == 'AMBIENT',]

LiCordata_adult <- na.omit(LiCordata_adult)
LiCordata_adult <- LiCordata_adult[,c(1:2,418:424,448,451)]
LiCordata_adult[, c(3:9)] <- sapply(LiCordata_adult[, c(3:9)], as.numeric)


LiCordata_adult1 <- LiCordata_adult1[,c(1:2,419:423,448,451)]
LiCordata_adult1[, c(3:7)] <- sapply(LiCordata_adult1[, c(3:7)], as.numeric)

#makes rowsname species names 
rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)


#created dataset for one site and all PFD readings
k <- setDT(LiCordata_adult, row.names(LiCordata_adult))
k <- dcast(k, k$BETPAP01 ~ k$Site,
           value.var = c("PFD-B", "PFD-FR", "PFD-G", "PFD-R", "PFD-UV"))
l <- LiCordata_adult1

l2<- l %>% group_by(l$Site) %>% summarise_all(funs(mean))

l2$Species <- rep("AMBIENT", 6)
l2 <- l2[,c(-3, -10)]
l2 <- setDT(l2, row.names(l2$Species))
l2 <- l2[,-1]
l2 <- dcast(l2, l2$Species ~ l2$`l$Site`,
           value.var = c("PFD-B", "PFD-FR", "PFD-G", "PFD-R", "PFD-UV"))


#creates rownames which are species codes
str(k)
k <- as.data.frame(k)
rownames(k) <- make.names(k$k)
k <- k[,-1]
l2 <- as.data.frame(l2)


path <- unique(LiCordata_adult1$Site)


for (i in 1:length(path)){
  k1 <- select(k, contains(path[i])) #selects variables that contain the sites 
  k1 <- na.omit(k1) #omits NAs
  l3 <- select(l2, contains(path[i])) #selects variables that contain the sites 
 path2 <- unique(names(k1))
 k2 <- (matrix(NA, nrow= nrow(k1), ncol = ncol(k1)))
 colnames(k2) <- colnames(k1)
 row.names(k2) <- rownames(k1)
for (n in 1:length(path2)){ 
  k2[,n]<- as.matrix(k1[,n] - l3[,n])
}  
  community_matrix <- as.matrix(k2) #creates matrix
  example_NMDS=metaMDS(community_matrix, k=3) #runs NMDS
  plotname <- paste("NMDS_ambspie",path[i],".pdf", sep="") #plotseach site seperately 
  pdf(file= plotname)
  stressplot(example_NMDS)
  plot(example_NMDS)
  plot(example_NMDS, main="Species Ordination", type="t", display="species")
  plot(example_NMDS)
  orditorp(example_NMDS,display="sites",cex= 0.5,air= 0.3)#labels points as best as we can
  dev.off()
}


