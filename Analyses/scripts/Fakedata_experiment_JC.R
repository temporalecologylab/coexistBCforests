#code to simulate fake data based on experimental design 
#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/scripts")

library(ggplot2)
library(ggpubr)
library(wesanderson)

set.seed(1234)

#I plan to run an experiment with four treatments and a three way interactions : soil_type, temp_treat, density & soil_treat
#all treatments are coded as binary and each treatment stands for the following:
#soil_type 0 = hetrospecific soil
#soil_type 1 = conspecific soil  
#soil_treat 0 = unsterilized soil
#soil_treat 1 = sterilized soil
#density 0 = single species
#density 1 = 5 conspecific species 
#temp_treat 0 = low temperature ~10 degrees
#temp_treat 1= high temperature ~25 degrees

### code the fake data

#### codes for the four treatments
density <- c(0,1)
temptreat <- c(0,1)
soil_type <- c(0,1)
soil_treat <- c(0,1)
error <- 7.5


### codes for intercept and individual effects
#y = biomass
intercepthere <- 50
soiltype_effect <- -15 #loses 15 units of biomass in conspecific soil
density_effect <- -7.5 # as in you LOSE 7.5 units of biomass in the high treatment
temp_effect <- 5 # as in you Gain 5 units of biomass in the high temp treatment
soiltreat_effect <- 2.5 # as in you Gain 2.5 units of biomass in the sterilized streatment


#### codes for interactions
densoiltype_intxn <- -5 #high density + conspecific soil causes a loss of 5 units of biomass
denstemp_intxn <- -2 # as in when you have high density and high temp you lose 2 units of biomass
densoiltreat_itxn <- -4 #high density + sterilization causes a Loss of 4 units of biomass
tempsoiltype_intxn <- -10 #high temp + conspecific soil causes a loss of 10 units of biomass
tempsoiltreat_intxn <- 2.5 #high temp + sterilization causes a gain of 2.5 units of biomass
soiltypesoiltreat_intxn <- 5 #conspecific soil + sterilization causes a gain of 5 unites of biomass
densoiltypetemp_intxn <- -2.5 #high temp + high density + conspecific soil causes a loss of 2.5 units of biomass
densoiltreattemp_intxn <- -2 #high temp + high density + sterilzation causes a loss of 0.5 units of biomass
densoiltypesoiltreat_intxn <- -4 #high density + conspecific soil + sterilization causes a loss of 4 units of biomass
soiltreattempsoiltype_intxn <-2.5 #high temp + conspecific soil + sterilization causes a gain of 2.5 units of biomass
all_intxn <- -2 #high temp + high density + conspecific soil + sterilization causes a loss of 2 units of biomass


#codes for number of replicates
reps_per_treatment <- 100 # 10 is always a good place to start (easy to do the math in your head and check your code)
ntot <- length(density)*length(temptreat)*length(soil_type)*length(soil_treat)*reps_per_treatment

#creates datae frame for a factorial design including all four treatments 
factorialgrid <- expand.grid(x_density = c(0, 1), x_temp = c(0,1), x_soiltype= c(0,1), x_soiltreat= c(0,1))

#codes a data frame based on factorial grid and number of reps
df <- as.data.frame(factorialgrid[rep(seq_len(nrow(factorialgrid)), each = reps_per_treatment),])

#calculates the biomass based on treatments and interactions 
df$y_biomass <- intercepthere +
  density_effect*df$x_density +
  temp_effect*df$x_temp +
  soiltype_effect*df$x_soiltype +
  soiltreat_effect*df$x_soiltreat + # 4 main effects
  densoiltype_intxn * (df$x_density * df$x_soiltype) +
  denstemp_intxn * (df$x_density * df$x_temp) +
  densoiltreat_itxn * (df$x_density * df$x_soiltreat) +
  tempsoiltreat_intxn * (df$x_temp * df$x_soiltreat) +
  tempsoiltype_intxn * (df$x_temp * df$x_soiltype) +
  soiltypesoiltreat_intxn * (df$x_soiltype * df$x_soiltreat) + # 6 2-way intxns
  densoiltypetemp_intxn * (df$x_density * df$x_soiltype * df$x_temp) +
  densoiltreattemp_intxn * (df$x_density * df$x_soiltreat * df$x_temp) +
  densoiltypesoiltreat_intxn * (df$x_density * df$x_soiltype * df$x_soiltreat) +
  soiltreattempsoiltype_intxn * (df$x_soiltreat * df$x_temp * df$x_soiltype ) + #4 3-way intxns
  all_intxn * (df$x_density * df$x_temp * df$x_soiltype * df$x_soiltreat)  # 1 4-way intxn

# incorporating 15% error into y
df$y_pred <- rnorm(n = ntot, mean = df$y_biomass, sd = error)

#Statistical model to test paramaters
fittedmodel <- lm(y_pred ~ x_density * x_temp * x_soiltype * x_soiltreat, data= df)

## Putting parameters into a list so you can call all of them at once
#suggestion from Geoff!
param <- list(intercepthere = 50,
              soiltype_effect = -15, 
              density_effect = -7.5, 
              temp_effect = 5, 
              soiltreat_effect = 2.5, 
              #### codes for interactions
              densoiltype_intxn = -5, 
              denstemp_intxn = -2, 
              densoiltreat_itxn = -4, 
              tempsoiltype_intxn = -10, 
              tempsoiltreat_intxn = 2.5, 
              soiltypesoiltreat_intxn = 5, 
              densoiltypetemp_intxn = -2.5, 
              densoiltreattemp_intxn = -2, 
              densoiltypesoiltreat_intxn = -4,
              soiltreattempsoiltype_intxn = 2.5, 
              all_intxn = -2)

## Model estimates
t(coef(fittedmodel))
## True value
t(param)
### Looks good

## What is estimated sigma?
sigma(fittedmodel)
## True sigma
error

write.csv(df, "fakedata_JC.csv")

fakedata_JC <- read_csv("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/output/fakedata_JC.csv")

#creates dataframe with mean values of biomass across parameters
plotdf<- aggregate(y_pred ~ parameter, fakedata_JC, mean)

Plot<- ggplot(plotdf,aes(parameter,y_pred, color=parameter)) + geom_bar(stat="identity", fill="white") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #causes x-axis labels to be vertical 

#fixes Y-axis label and removes figure legend 
plot1.1<-print(Plot + labs(y="Biomass (g)") + theme(legend.position = "none"))


for (i in 1:1600){
if (df$x_density[i]== 0){
  df$x_density[i] <- "low"
} else {
  df$x_density[i] <- "high"
  
}
}

for (i in 1:1600){
  if (df$x_temp[i]== 0){
    df$x_temp[i] <- "low temperature"
  } else {
    df$x_temp[i] <- "high temperature"
    
  }
}

for (i in 1:1600){
  if (df$x_soiltype[i]== 0){
    df$x_soiltype[i] <- "heterospecific"
  } else {
    df$x_soiltype[i] <- "conspecific"
    
  }
}

for (i in 1:1600){
  if (df$x_soiltreat[i]== 0){
    df$x_soiltreat[i] <- "unsterilized soil"
  } else {
    df$x_soiltreat[i] <- "sterilized soil"
    
  }
}


df$x_density <- as.factor(df$x_density)
df$x_temp <- as.factor(df$x_temp )
df$x_soiltype <- as.factor(df$x_soiltype)
df$x_soiltreat <- as.factor(df$x_soiltreat)

colnames(df)[1] <- "density"
colnames(df)[2]<- "temperature"
colnames(df)[3]<- "soil type"
colnames(df)[4]<- "soil treatment"



JC<- ggplot(df, aes(x=density, y=y_pred, color= df$`soil type`, fill= df$`soil treatment`)) + geom_boxplot() + facet_grid(~ temperature) + 
  labs(y="Biomass (g)", colour= "soil type", fill= "soil treatment") + theme(legend.position="right") + scale_fill_brewer(palette="PRGn") + scale_color_brewer(palette="Set1")

