#Power analysis for experimental design 
##Started by Darwin on June 17th 2020
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

library(pwr)
library(tidyverse)
library(Hmisc)
set.seed(1993)

#range of effect size
r <- seq(0.01,1,0.01)
nr <- length(r)

#power values
p <- seq(0.1,1,0.1)
np <- length(p)

#obtain sample sizes
samesize <- array(numeric(nr*np), dim=c(nr,np))
for (i in seq(p)){
  for (j in seq(r)){
    result <- pwr.f2.test(u= 3, f2= r[j], sig.level = 0.05, power= p[i])
    samesize[j,i] <- result$u + result$v + 1
  }
}

# set up graph
xrange <- range(r)
samesize <- samesize[,-10]
yrange <- round(range(samesize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (f2)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samesize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05")
legend("topright", title="Power", as.character(p),
       fill=colors)

###### Alternative approach 
#Linear model is biomass ~ soil type + sterilization + density
# biomass= intercept + soil type + sterilization + density

intercept <- -50
soiltype <- 75
sterilization <- 25
density <- 50


#soiltype == 0 = conspecific
#soiltype == 1 = heterspecific
#sterilization == 0 = unsterilized soil
#sterilization == 1 = sterilized soil
#density == 0 = high density 
#density == 1 = low density

#Generating data for parameters 
n <- 10000
samplep<- sample(c(0,1), replace=TRUE, size=n)
samplesoil <- as.data.frame(sample(samplep, replace = FALSE, size=10000))
samplesterilization <- as.data.frame(sample(samplep, replace = FALSE, size=10000))
sampledensity <- as.data.frame(sample(samplep, replace = FALSE, size=10000))


#code for model

HVOB <- (matrix(NA, nrow= nrow(samplesoil), ncol = 5))
for (n in 1:10000){
  HVOB[n,1]<- as.matrix(intercept + soiltype * samplesoil[n,] + sterilization * samplesterilization[n,]
                       + density * sampledensity[n,])
  HVOB[n,2]<- as.matrix(intercept)
  HVOB[n,3] <- as.matrix(soiltype * samplesoil[n,])
  HVOB[n,4] <- as.matrix(sterilization * samplesterilization[n,])
  HVOB[n,5] <- as.matrix(density * sampledensity[n,])
  
} 

HVOB <- as.data.frame(HVOB) 
 HVOB <- HVOB %>%
  rename(
    Biomass = V1 ,
    intercept = V2,
    soiltype = V3,
    sterilization = V4,
    density = V5
  )

sd(HVOB$Biomass)
mean(HVOB$Biomass)

data10 <- as.data.frame(rnorm(n = 10, mean = HVOB[1:10, 1], sd = 47))
test10<- cbind(HVOB[1:10,],data10)
test10 <- test10 %>%
  rename(
  data  = 'rnorm(n = 10, mean = HVOB[1:10, 1], sd = 47)'
  )
lm(Biomass~ data, data = test10)
summary(Biomass~ data, data = test10)


data100 <- as.data.frame(rnorm(n = 100, mean = HVOB[1:100, 1], sd = 47))
test100 <- cbind(HVOB[1:100,],data100)
test100 <- test100 %>%
  rename(
    data  = 'rnorm(n = 100, mean = HVOB[1:100, 1], sd = 47)'
  )

lm(Biomass~ data, data = test100)
summary(Biomass~ data, data = test100)


data1000 <- as.data.frame(rnorm(n = 1000, mean = HVOB[1:1000, 1], sd = 47))
test1000 <- cbind(HVOB[1:1000,],data1000)
test1000 <- test1000 %>%
  rename(
    data  = 'rnorm(n = 1000, mean = HVOB[1:1000, 1], sd = 47)'
  )

lm(Biomass~ data, data = test1000)
summary(Biomass~ data, data = test1000)









data10000 <- rnorm(n = 1000, mean = HVOB[1:10000, 1], sd = 47)
