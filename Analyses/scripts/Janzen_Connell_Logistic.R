#Janzen Connell Logistic regression
# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/") # setwd("~/Documents/git/projects/others/darwin/coexistencebc/Analyses")

library(readxl)
library(dplyr)
library(ggplot2)

Janzen_Connell_ <- read_excel("..//Data/Janzen-Connell /Janzen-Connell .xlsx")

################################################################################
#Filters data where health scale is either equal to 1 or 5
Janzen_Connell_Logistic <- filter(Janzen_Connell_, health_scale == 1 | health_scale  == 5)

for (n in 1:355){
  
  #if statement extracts all agricultural species in that genus if a species name is not given
  if (Janzen_Connell_Logistic$health_scale[n] == 5){
    Janzen_Connell_Logistic$health_scale[n] <- 1
  } else {
    Janzen_Connell_Logistic$health_scale[n] <- 0
  }
}


#creates a new column conspecific where if health scale is 5
#then 1 get added otherwise 0 gets added
for (n in 1:355){
  
  if (Janzen_Connell_Logistic$Species[n] == Janzen_Connell_Logistic$Closest_adult[n]){
    Janzen_Connell_Logistic$conspecific[n] <- 1
  } else {
    Janzen_Connell_Logistic$conspecific[n] <- 0
  }
}

apparat<- glm(health_scale ~ conspecific,family=binomial(link='logit'),data=Janzen_Connell_Logistic)

summary(apparat)

plotname <- paste("output/JC_logistic",".pdf", sep="")
pdf(file= plotname)
print(ggplot(Janzen_Connell_Logistic, aes(x=conspecific, y=health_scale, color=Species)) +
  geom_point(position = position_jitter(height = 0.05, width = 0.1)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE))
dev.off()

#With confidence intervals
apparat2<- ggplot(Janzen_Connell_Logistic, aes(x=conspecific, y=health_scale, color=Species)) +
  geom_point(position = position_jitter(height = 0.05, width = 0.1)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE, level= 0.8)
         
#PICENG/PICGLA don't have a lot of data
#TSUHET shows a very weak trend where health decreases while near conspecific
#POPTRE shows a strong tread where health decreases while near conspecific
#ACEGLA shows opposite trend where health increases while near conspecific

