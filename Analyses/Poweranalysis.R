#Power analysis for experimental design 
##Started by Darwin on June 17th 2020
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Coexistence-in-BC-Forests/Analyses/")

library(pwr)

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

