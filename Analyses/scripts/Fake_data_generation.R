#code to simulate fake data based on experimental design 
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/scripts")


set.seed(1234)

y = biomass
soil_type <- c(0,1)
soil_treatment <- c(0,1)
density <- c(0,1)
temp <- c(0,1)

#does this make sense or should I code variables seperately?

#soil_type 0 = hetrospecific 
#soil_type 1 = conspecific  
#soil_treatment 0 = unsterilized
#soil_treatment 1 = sterilized 
#density 0 = single species
#density 1 = 5 conspecific species 
#temp 0 = high temperature ~25 degrees
#temp 1 = low temperature ~10 degrees

#biomass = intercept + soil_type + soil_treatment + density + temp * interactions of all paramteres

#random numbers given to paramters based on predictions
intercept = 30 
soil_type_test= -10
soil_treatment_test = -5
density_test = -15
temp_test = -7.5 

nreplicates<- 100
london_grammar<-data.frame(matrix(NA, nrow= nreplicates))
for(i in 1:nreplicates){
  soil_type_loop <- sample(soil_type, i, replace=TRUE)
  soil_treatment_loop <- sample(soil_treatment, i, replace=TRUE)
  density_loop <- sample(density, i, replace=TRUE)
  temp_loop <-sample(temp, i, replace=TRUE) 
  london_grammar[i,] <- intercept + (soil_type_test * soil_type_loop[i]) + (soil_treatment_test * soil_treatment_loop[i]) +
    (density_test * density_loop[i]) + (temp_test * temp_loop[i])
  
}

#how to figure out interaction values?
#how to include these into the model?
#does this even make sense?


