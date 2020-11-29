#code to simulate fake data based on experimental design 
#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/scripts")

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

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
soiltype_effect <- 10 #gains 10 units of biomass in conspecific soil
density_effect <- -5 # as in you LOSE 5 units of biomass in the high treatment
temp_effect <- 5 # as in you gain 5 units of biomass in the high temp treatment
soiltreat_effect <- 2.5 # as in you Gain 2.5 units of biomass in the sterilized streatment


#### codes for interactions
densoiltype_intxn <- 2.5 #high density + conspecific soil causes a gain of 2.5 units of biomass
denstemp_intxn <- 2 # as in when you have high density and high temp you gain 2 units of biomass
densoiltreat_itxn <- -5 #high density + sterilization causes a Loss of 5 units of biomass
tempsoiltype_intxn <- 2.5 #high temp + conspecific soil causes a gain of 2.5 units of biomass
tempsoiltreat_intxn <- 1 #high temp + sterilization causes a gain of 1 units of biomass
soiltypesoiltreat_intxn <- 2.5 #conspecific soil + sterilization causes a gain of 2.5 unites of biomass
densoiltypetemp_intxn <- 5  #high temp + high density + conspecific soil causes a gain of 5 units of biomass
densoiltreattemp_intxn <- -1.5 #high temp + high density + sterilzation causes a loss of 1.5 units of biomass
densoiltypesoiltreat_intxn <- -5  #high density + conspecific soil + sterilization causes a loss of 5 units of biomass
soiltreattempsoiltype_intxn <-1 #high temp + conspecific soil + sterilization causes a gain of 1 units of biomass
all_intxn <- -2.5 #high temp + high density + conspecific soil + sterilization causes a loss of 2.5 units of biomass


#codes for number of replicates
reps_per_treatment <- 100 # 10 is always a good place to start (easy to do the math in your head and check your code)
ntot <- length(density)*length(temptreat)*length(soil_type)*length(soil_treat)*reps_per_treatment

#creates datae frame for a factorial design including all four treatments 
factorialgrid <- expand.grid(x_density = c(0, 1), x_temp = c(0,1), x_soiltype= c(0,1), x_soiltreat= c(0,1))

#codes a data frame based on factorial grid and number of reps
df2.0 <- as.data.frame(factorialgrid[rep(seq_len(nrow(factorialgrid)), each = reps_per_treatment),])

#calculates the biomass based on treatments and interactions 
df2.0$y_biomass <- intercepthere +
  density_effect*df2.0$x_density +
  temp_effect*df2.0$x_temp +
  soiltype_effect*df2.0$x_soiltype +
  soiltreat_effect*df2.0$x_soiltreat + # 4 main effects
  densoiltype_intxn * (df2.0$x_density * df2.0$x_soiltype) +
  denstemp_intxn * (df2.0$x_density * df2.0$x_temp) +
  densoiltreat_itxn * (df2.0$x_density * df2.0$x_soiltreat) +
  tempsoiltreat_intxn * (df2.0$x_temp * df2.0$x_soiltreat) +
  tempsoiltype_intxn * (df2.0$x_temp * df2.0$x_soiltype) +
  soiltypesoiltreat_intxn * (df2.0$x_soiltype * df2.0$x_soiltreat) + # 6 2-way intxns
  densoiltypetemp_intxn * (df2.0$x_density * df2.0$x_soiltype * df2.0$x_temp) +
  densoiltreattemp_intxn * (df2.0$x_density * df2.0$x_soiltreat * df2.0$x_temp) +
  densoiltypesoiltreat_intxn * (df2.0$x_density * df2.0$x_soiltype * df2.0$x_soiltreat) +
  soiltreattempsoiltype_intxn * (df2.0$x_soiltreat * df2.0$x_temp * df2.0$x_soiltype ) + #4 3-way intxns
  all_intxn * (df2.0$x_density * df2.0$x_temp * df2.0$x_soiltype * df2.0$x_soiltreat)  # 1 4-way intxn

# incorporating 15% error into y
df2.0$y_pred <- rnorm(n = ntot, mean = df2.0$y_biomass, sd = error)

#Statistical model to test paramaters
fittedmodel <- lm(y_pred ~ x_density * x_temp * x_soiltype * x_soiltreat, data= df2.0)

## Putting parameters into a list so you can call all of them at once
#suggestion from Geoff!
param <- list(intercepthere = 50,
              soiltype_effect = -10, #loses 10 units of biomass in conspecific soil
              density_effect = -5, # as in you LOSE 5 units of biomass in the high treatment
              temp_effect = -7.5, # as in you LOSE 7.5 units of biomass in the low temp treatment
              soiltreat_effect = 5, # as in you Gain 10 units of biomass in the sterilized streatment
              #### codes for interactions
              densoiltype_intxn = -12, #high density + conspecific soil causes a loss of 12 units of biomass
              denstemp_intxn = 2, # as in when you have high density and low temp you GAIN 2 units of biomass
              densoiltreat_itxn = -1, #high density + sterilization causes a Loss of one unit of biomass
              tempsoiltype_intxn = -4, #low temp + conspecific soil causes a loss of 4 units of biomass
              tempsoiltreat_intxn = -2.5, #low temp + sterilization causes a loss of 2.5 units of biomass
              soiltypesoiltreat_intxn = -1.5, #conspecific soil + sterilization causes a loss of 1.5 unites of biomass
              densoiltypetemp_intxn = -0.5, #low temp + high density + conspecific soil causes a loss of 0.5 units of biomass
              densoiltreattemp_intxn = 1, 
              densoiltypesoiltreat_intxn = 2.5,
              soiltreattempsoiltype_intxn = 0.5, 
              all_intxn = -0.75)

## Model estimates
t(coef(fittedmodel))
## True value
t(param)
### Looks good

## What is estimated sigma?
sigma(fittedmodel)
## True sigma
error

write.csv(df2.0, "fakedata_MF.csv")

fakedata_MF <- read_csv("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/output/fakedata_MF.csv")

#creates dataframe with mean values of biomass across parameters
plotdf2.02.0<- aggregate(y_pred ~ parameter, fakedata_MF, mean)

Plot2.0<- ggplot(plotdf2.02.0,aes(parameter,y_pred, color=parameter)) + geom_bar(stat="identity", fill="white") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #causes x-axis labels to be vertical 

#fixes Y-axis label and removes figure legend 
Plot2.1<-print(Plot2.0 + labs(y="Biomass (g)") + theme(legend.position = "none"))

for (i in 1:1600){
  if (df2.0$x_density[i]== 0){
    df2.0$x_density[i] <- "low"
  } else {
    df2.0$x_density[i] <- "high"
    
  }
}

for (i in 1:1600){
  if (df2.0$x_temp[i]== 0){
    df2.0$x_temp[i] <- "low temperature"
  } else {
    df2.0$x_temp[i] <- "high temperature"
    
  }
}

for (i in 1:1600){
  if (df2.0$x_soiltype[i]== 0){
    df2.0$x_soiltype[i] <- "heterospecific"
  } else {
    df2.0$x_soiltype[i] <- "conspecific"
    
  }
}

for (i in 1:1600){
  if (df2.0$x_soiltreat[i]== 0){
    df2.0$x_soiltreat[i] <- "unsterilized soil"
  } else {
    df2.0$x_soiltreat[i] <- "sterilized soil"
    
  }
}


df2.0$x_density <- as.factor(df2.0$x_density)
df2.0$x_temp <- as.factor(df2.0$x_temp )
df2.0$x_soiltype <- as.factor(df2.0$x_soiltype)
df2.0$x_soiltreat <- as.factor(df2.0$x_soiltreat)

colnames(df2.0)[1] <- "density"
colnames(df2.0)[2]<- "temperature"
colnames(df2.0)[3]<- "soil type"
colnames(df2.0)[4]<- "soil treatment"



MF<-ggplot(df2.0, aes(x=density, y=y_pred, color= df2.0$`soil type`, fill= df2.0$`soil treatment`)) + geom_boxplot() + facet_grid(~ temperature) + 
  labs(y="Biomass (g)", colour= "soil type", fill= "soil treatment") + theme(legend.position="right") + scale_fill_brewer(palette="PRGn") + scale_color_brewer(palette="Set1")






figure <- ggarrange(JC, MF,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


