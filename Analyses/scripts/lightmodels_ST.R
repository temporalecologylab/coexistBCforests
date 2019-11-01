####script to create light data for Ph.D. 
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

library(vegan)
library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)
library(ggplot2)

LiCordatafinal <- read_csv("input/LiCordatafinalpsp.csv")

LiCordata_adult <- LiCordatafinal[LiCordatafinal$Species != 'AMBIENT',]

LiCordata_adult1 <- LiCordatafinal[LiCordatafinal$Species == 'AMBIENT',]

LiCordata_adult <- LiCordata_adult[,c(1:403,441,450)]
LiCordata_adult[, c(3:403)] <- sapply(LiCordata_adult[, c(3:403)], as.numeric)

LiCordata_adult1 <- LiCordata_adult1[,c(1:403,441,450)]
LiCordata_adult1[, c(3:403)] <- sapply(LiCordata_adult1[, c(3:403)], as.numeric)

LiCordata_adult <- LiCordata_adult[order(LiCordata_adult$Time),]

LiCordata_adult <- LiCordata_adult[-54,]


#makes rowsname species names 
rownames(LiCordata_adult) <- make.names(LiCordata_adult$Species, unique = TRUE)

#repeats each ambient reading three times in a new dataset
LiCordata_adult1 <- LiCordata_adult1 %>% slice(rep(1:n(), each = 3))

path <- unique(names(LiCordata_adult[,(3:403)]))
k2 <- (matrix(NA, nrow= nrow(LiCordata_adult[,(3:403)]), ncol = ncol(LiCordata_adult[,(3:403)])))
colnames(k2) <- colnames(LiCordata_adult[,(3:403)])
row.names(k2) <- rownames(LiCordata_adult)
for (n in 1:length(path)){ 
  k2[,n]<- as.matrix(LiCordata_adult[,(3:403)][,n] - LiCordata_adult1[,(3:403)][,n])
}  
community_matrix <- as.matrix(k2) #creates matrix
example_NMDS=metaMDS(community_matrix, k=3, autotransform = FALSE) #runs NMDS
filename <- paste("output/NMDS_RambspcPSPST",".pdf", sep = )
pdf(file= filename)
write.csv(k2, filename)
stressplot(example_NMDS)
plot(example_NMDS)
orditorp(example_NMDS,display="sites",cex= 0.5,air= 0.3)#labels points as best as we can
dev.off()

k2 <- as.data.frame(k2)
k2 <- rownames_to_column(k2)
write_csv(k2, "NMDS_RambspcPSPST3.csv")


k2<- melt(k2[,c(1:402)], id.vars = c("rowname"))

k2$species<-sub("^([[:alpha:]]*).*", "\\1", k2$rowname)

k2 <- k2[,-1]

k2$variable<- gsub("nm","", paste(k2$variable))
k2$variable <- as.numeric(k2$variable)

drake <- subset(k2, species== "THUPLI")
plot(value ~ variable , data= drake)
plot(value ~ variable , data= li)

ggplot(data = k2, aes(variable,value,color=species)) + geom_point()
ggplot(data=k2, aes(species,value,)) + geom_boxplot()

k3 <- k2 %>%
  group_by(variable,species) %>%
  summarise(avg = mean(value))

ggplot(data = k3, aes(variable,avg,color=species)) + geom_point()


path <- unique(k2$species)

for (i in (path)){
  filename<- paste("output/Species_models_ST",i,".pdf", sep = "")
  pdf(file= filename)
  li <- subset(k2, species == i)
  li <- na.omit(li)
  plot(value ~ variable, data= li)
  dev.off()
}

path2<- unique(k3$species)


for (i in (path2)){
  filename<- paste("output/Species_models_ST2",i,".pdf", sep = "")
  pdf(file= filename)
  li <- subset(k3, species == i)
  li <- na.omit(li)
  plot(avg ~ variable, data= li)
  dev.off()
}
