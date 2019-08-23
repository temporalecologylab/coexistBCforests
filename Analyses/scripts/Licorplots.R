rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

library(vegan)
library(ape)
library(dplyr)
library(factoextra)

LiCordatafinal <- read_csv("input/LiCordatafinal.csv")

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

         
#don't quite know what this plot shows          
pairs(LiCordata_adult[, c(3:9)], 
      lower.panel= NULL)

#creating a pca
LiCordata_adult.pca <- princomp(LiCordata_adult[,3:9])
biplot(LiCordata_adult.pca)

LiCordata_adult.pca2 <- prcomp(LiCordata_adult[,3:9], scale. = TRUE)
fviz_eig(LiCordata_adult.pca2)

fviz_pca_ind(LiCordata_adult.pca2,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

fviz_pca_var(LiCordata_adult.pca2,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_biplot(LiCordata_adult.pca2, repel = TRUE, 
                col.var = "#2E9FDF", 
                col.ind = "#696969")

#eigenvalues
eig.val <- get_eigenvalue(LiCordata_adult.pca2)

#results for Variables
res.var <- get_pca_var(LiCordata_adult.pca2)

#Results for individuals
res.ind <- get_pca_ind(LiCordata_adult.pca2)


#creating a rda for site differences 
LiCordata_adult.rda <- rda(LiCordata_adult[,3:9])
biplot(LiCordata_adult.rda,
       display = c("sites", "species"),
       type = c("text", "points"))



biplot(LiCordata_adult.rda,
       display = c("sites", "species"),
       type = c("text", "points"))

site.names <- unique(LiCordata_adult$Site)

ordihull(LiCordata_adult.rda,
         group = LiCordata_adult$Site,
         col= c(1:10))

legend(35, 20,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = site.names,
       title = "Site",
       xpd = TRUE)


#creating pca for species differences 

biplot(LiCordata_adult.rda,
       display = c("sites", "species"),
       type = c("text", "points"))

spp.names <- unique(LiCordata_adult$specieslatbi)

ordihull(LiCordata_adult.rda,
         group = LiCordata_adult$specieslatbi,
         col= c(1:8))

legend(35, 28,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = spp.names,
       title = "Species",
       xpd = TRUE)


#creating pca for day of month

biplot(LiCordata_adult.rda,
       display = c("sites", "species"),
       type = c("text", "points"))

DOM<- unique(LiCordata_adult$day)

ordihull(LiCordata_adult.rda,
         group = LiCordata_adult$day,
         col= c(1:3))

legend(35, 28,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = DOM,
       title = "Day of Month",
       xpd = TRUE)


#subsetting dataset for adult trees only!
LiCordata_adult2 <- LiCordatafinal[LiCordatafinal$seedlingoradult == 'a',]
LiCordata_adult2 <- na.omit(LiCordata_adult2)
LiCordata_adult2 <- LiCordata_adult2[,c(1:2,426:440,448,451)]
LiCordata_adult2[, c(3:17)] <- sapply(LiCordata_adult2[, c(3:17)], as.numeric)

LiCordata_adult.pca3 <- prcomp(LiCordata_adult2[,3:17], scale. = TRUE)

#eigenvalues
eig.val <- get_eigenvalue(LiCordata_adult.pca3)

#results for Variables
res.var <- get_pca_var(LiCordata_adult.pca3)

#Results for individuals
res.ind <- get_pca_ind(LiCordata_adult.pca3)

#creating a rda for site differences 
LiCordata_adult.rda2 <- rda(LiCordata_adult2[,3:17])
biplot(LiCordata_adult.rda2,
       display = c("sites", "species"),
       type = c("text", "points"))



biplot(LiCordata_adult.rda2,
       display = c("sites", "species"),
       type = c("text", "points"))

site.names <- unique(LiCordata_adult2$Site)

ordihull(LiCordata_adult.rda2,
         group = LiCordata_adult2$Site,
         col= c(1:10))

legend(5, 5,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = site.names,
       title = "Site",
       xpd = TRUE)


#creating pca for species differences 

biplot(LiCordata_adult.rda2,
       display = c("sites", "species"),
       type = c("text", "points"))

spp.names <- unique(LiCordata_adult2$specieslatbi)

ordihull(LiCordata_adult.rda2,
         group = LiCordata_adult2$specieslatbi,
         col= c(1:8))

legend(5, 5,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = spp.names,
       title = "Species",
       xpd = TRUE)


#creating pca for day of month

biplot(LiCordata_adult.rda2,
       display = c("sites", "species"),
       type = c("text", "points"))

DOM<- unique(LiCordata_adult2$day)

ordihull(LiCordata_adult.rda2,
         group = LiCordata_adult2$day,
         col= c(1:3))

legend(5, 5,  
       col = c(1:10), 
       inset=c(-0.2,0),
       cex= 0.45, 
       lty = 1,
       pch= 1 ,
       legend = DOM,
       title = "Day of Month",
       xpd = TRUE)




#Creating dataset for boxplots
Boxplot.Species <- LiCordata_adult[,c(3:9, 11)]
Boxplot.Species2 <- as.data.frame(Boxplot.Species)
Boxplot.site <- LiCordata_adult[,c(2,3:9)]
Boxplot.Day <- LiCordata_adult[,c(3:9, 10)]
LiCordata_adult[, 10] <- sapply(LiCordata_adult[, 10], as.character)
Boxplot.Day[,8] <- sapply(Boxplot.Day[,8], as.character)



for (i in 1:2){
  plotname <- paste("boxplots_for_species_vs_FD_readings",colnames(Boxplot.Species2[i]),".pdf")
  pdf(file= plotname)
  ggplot(Boxplot.Species2, aes(x=specieslatbi, y=Boxplot.Species2[,i])) + geom_boxplot()
  dev.off()
} 



#boxplots for species vs PFD readings (tried to make a for loop to do this but failed :( )
pdf('file=boxplots_for_species_vs_FD_readings.pdf')
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$PPFD)) + geom_boxplot()
dev.off()


#boxplots for Site vs PFD readings (tried to make a for loop to do this but failed :( )
pdf('file=boxplots_for_site_vs_FD_readings.pdf')
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$PPFD)) + geom_boxplot()
dev.off()


#boxplots for Day of month vs PFD readings (tried to make a for loop to do this but failed :( )
pdf('file=boxplots_for_day_vs_FD_readings.pdf')
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$PPFD)) + geom_boxplot()
dev.off()


