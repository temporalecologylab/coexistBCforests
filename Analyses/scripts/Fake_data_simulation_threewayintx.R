#code to simulate fake data based on experimental design for a two way interaction
#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/scripts")

set.seed(1234)

#I plan to run an experiment with four treatments and a three way interactions : soil_type, temp_treat, density & soil_treat
#all treatments are coded as binary and each treatment stands for the following:
#soil_type 0 = hetrospecific soil
#soil_type 1 = conspecific soil  
#soil_treat 0 = unsterilized soil
#soil_treat 1 = sterilized soil
#density 0 = single species
#density 1 = 5 conspecific species 
#temp_treat 0 = high temperature ~25 degrees
#temp_treat 1= low temperature ~10 degrees

### code the fake data
density <- c(0,1)
temptreat <- c(0,1)
soil_type <- c(0,1)
intercepthere <- 30  # watch out of potentially reserved words in R ..
# I don't think 'intercept' or 'temp' are used in base R, but they might be in other packages. 

#coding effects
density_effect <- -5 # as in you LOSE 5 units of biomass in the high treatment
temp_effect <- -7.5 # as in you LOSE 7.5 units of biomass in the high temp treatment
denstemp_intxn <- 2 # as in when you have high density and high temp you GAIN 2 units of biomass
soiltype_effect <- -10 #loses 10 units of biomass in conspecific soil
densoil_intxn <- -12 #high density + conspecific soil causes a loss of 12 units of biomass
tempsoil_intxn <- -4 #high temp + conspecific soil causes a loss of 4 units of biomass
densoiltemp_intxn <- -1#high temp + high density + conspecific soil causes a loss of 1 units of biomass

reps_per_treatment <- 100 # 10 is always a good place to start (easy to do the math in your head and check your code)

#finds out number of rows needed based on length of all paramters and reps
ntot <- length(density)*length(temptreat)*length(soil_type)*reps_per_treatment

#creates a grid for the factorial design of experiment
factorialgrid <- expand.grid(x_density = c(0, 1), x_temp = c(0,1), x_soil= c(0,1))

#creates dataset based on factorial grid and reps per treatment
df <- as.data.frame(factorialgrid[rep(seq_len(nrow(factorialgrid)), each = reps_per_treatment),])

#calculates the biomass based on treatments and interactions 
df$y_biomass <- intercepthere + 
  density_effect*df$x_density + 
  temp_effect*df$x_temp + 
  soiltype_effect*df$x_soil + #3 one-way interactions
  denstemp_intxn*(df$x_density*df$x_temp) + 
  densoil_intxn * (df$x_density*df$x_soil) + 
  tempsoil_intxn * (df$x_temp * df$x_soil) + #3 two way interactions
  densoiltemp_intxn * (df$x_density * df$x_temp * df$x_soil) #1 three way interaction

#incorporating error based on intercept value 
error <- 0.15 * 30

# incorporating 15% error into y
df$y_pred<- df$y_biomass + rnorm(ntot, error)

#Statistical model to test paramaters
lm(y_pred~ x_density * x_temp * x_soil, data= df)
