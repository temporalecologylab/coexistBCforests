#code to simulate fake data based on experimental design 
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()

# setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/scripts")


set.seed(1234)

if(FALSE){
# y = biomass
soil_type <- c(0,1)
soil_treatment <- c(0,1)
density <- c(0,1)
temp <- c(0,1)

# does this make sense or should I code variables seperately? (EMW: Either way works, it will depend on the rest of your code!)

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
}

# EMW: Good start through here! But a few things to work on:
# (1) Start with a simpler version of the experiment! (More on this below)
# (2) You need to build the full dataframe up, it should look in the end like 'real' data, both because otherwise it's too hard to think through (at least for me) and also because it's way easier to check your work.

# Here's the start of a simpler example (but still including an interaction): imagine you have an experiment with a low and high density treatment of ONLY conspecific and low and high temperature in a full factorial design ... following some of your code from above:

density <- c(0,1)
temptreat <- c(0,1)
intercepthere <- 30  # watch out of potentially reserved words in R ..
    # I don't think 'intercept' or 'temp' are used in base R, but they might be in other packages. 
density_effect <- -5 # as in you LOSE 5 units of biomass in the high treatment
temp_effect <- -7.5 # as in you LOSE 7.5 units of biomass in the high temp treatment
denstemp_intxn <- 2 # as in when you have high density and high temp you GAIN 2 units of biomass

reps_per_treatment <- 10 # 10 is always a good place to start (easy to do the math in your head and check your code)
ntot <- length(density)*length(temptreat)*reps_per_treatment

# Okay, so, I will need a dataframe with the following columns: density, temptreat (these are my two X data) and biomass (Y data)
# For building experimental data I use expand.grid to get the factorial right
factorialgrid <- expand.grid(x_density = c(0, 1), x_temp = c(0,1))

df <- as.data.frame(factorialgrid[rep(seq_len(nrow(factorialgrid)), each = reps_per_treatment),])
# check my work
df$treatcombo = paste(df$x_density, df$x_temp, sep = "_")
table(df$treatcombo) 

# now we just need to build the y data ... I think you have all you need now, see where you get.
df$biomass <- intercepthere + ...

# first stab 
nreplicates <- 100
london_grammar <- data.frame(matrix(NA, nrow= nreplicates))
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


