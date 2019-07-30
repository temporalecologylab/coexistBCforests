rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/LI-COR Data (Manning Park)")

library(vegan)
library(ape)
library(dplyr)

LiCordatafinal <- read.csv("LiCordatafinal.csv")

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
      lower.panel= NULL) # this shows how correlated each variable is to each other one ... for example in row 3, column 4 you see PFD.FR versus PFD.G

#creating a pca
LiCordata_adult.pca <- princomp(LiCordata_adult[,3:9])
biplot(LiCordata_adult.pca)

#looking at pcas
summary(LiCordata_adult.pca) ## this suggests to me something a little weird given all the variance is explained in first axis
summary(princomp(LiCordata_adult[,4:8])) # same thing (the pairs plot explains this -- everything is tightly correlated)

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

#Creating dataset for boxplots
Boxplot.Species <- LiCordata_adult[,c(3:9, 11)]
Boxplot.site <- LiCordata_adult[,c(2,3:9)]
Boxplot.Day <- LiCordata_adult[,c(3:9, 10)]
LiCordata_adult[, 10] <- sapply(LiCordata_adult[, 10], as.character)
Boxplot.Day[,8] <- sapply(Boxplot.Day[,8], as.character)



#boxplots for species vs PFD readings (tried to make a for loop to do this but failed :( )
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.Species, aes(x=specieslatbi, y=Boxplot.Species$PPFD)) + geom_boxplot()

#boxplots for Site vs PFD readings (tried to make a for loop to do this but failed :( )
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.site, aes(x=Site, y=Boxplot.Species$PPFD)) + geom_boxplot()

#boxplots for Day of month vs PFD readings (tried to make a for loop to do this but failed :( )
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-B`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$PFD)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-FR`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-G`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-R`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$`PFD-UV`)) + geom_boxplot()
ggplot(Boxplot.Day, aes(x=day, y=Boxplot.Species$PPFD)) + geom_boxplot()


