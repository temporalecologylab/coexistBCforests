#Started Sept 19 2023 by D Loughnan

# aim of this code is to:
# 1. explore and clean the data from several years of seedling survival observations
# 2. Make some plots to help us decide what our next steps should be.
rm(list=ls()) 

require(reshape2)
require(ggplot2)

if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/bccoexistence/")
} else {
setwd("~/Documents/github/coexistBCforests")} 

dat <- read.csv("Data/Janzen-Connell/SeedlingDATA2021-2023.csv",
                na.strings=c("NA","NaN", " ","") )

head(dat)

unique(dat$Germinant_ID)

noGerm <- aggregate(dat [c("Count_2020","Count_2021","Count_2022","Count_2023")],
          dat[c("Site","Plot")],
          FUN = sum)
names(noGerm) <- c("Site", "Plot","2020", "2021","2022","2023")

noGerm <- melt(noGerm, id.vars = c("Site", "Plot"))
noGerm$variable <- as.integer(as.character(noGerm$variable))

# Quick work below by Lizzie to look at 2024 data also
dat2024 <- read.csv("Data/Janzen-Connell/SeedlingDATA2020_2024.csv",
                na.strings=c("NA","NaN", " ","") )
# Hmm, the count column is all empty. I will work on this; see temporalecologylab/coexistBCforests/issues/2

# end work by Lizzie to look at 2024 data also

plot(noGerm$variable, noGerm$value, col = as.factor(noGerm$Plot))

noGerm$Plot <- as.factor(noGerm$Plot)

pdf("Analyses/figures/noGermTime.pdf", width = 5, height = 5)
ggplot(data = noGerm, aes(x=variable, y=value)) + geom_line(aes(colour=Plot))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Year",
       y = "No Germinants")
dev.off()

# Now let's look into the sub plots:
noGermSub <- aggregate(dat [c("Count_2020","Count_2021","Count_2022","Count_2023")],
                    dat[c("Site","Plot","Sub_plot")],
                    FUN = sum)
names(noGermSub) <- c("Site", "Plot","Sub_plot","2020", "2021","2022","2023")

noGermSub <- melt(noGermSub, id.vars = c("Site", "Plot","Sub_plot"))
noGermSub$variable <- as.integer(as.character(noGermSub$variable))
names(noGermSub) <- c("Site", "Plot","Sub_plot","Year", "Germ")
noGermSub$Plot <- as.numeric(noGermSub$Plot)

noGermSub1 <- subset(noGermSub, Plot < 9)
noGermSub1$Plot <- as.factor(noGermSub1$Plot)
noGermSub1$Sub_plot <- as.factor(noGermSub1$Sub_plot)

pdf("Analyses/figures/GermNoPart1Lumped.pdf", width = 3, height = 8)
p <- ggplot(data = noGermSub1, aes(x= Year, y= Germ)) + geom_line(aes(colour=Sub_plot)) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA)) +
  labs (y = "No Germinants",
        col = "Sub plot")
 p + facet_grid(vars(Plot)#, vars(Sub_plot))
                )+ theme(panel.spacing.x = unit(2, "lines"))
dev.off() 

noGermSub2 <- subset(noGermSub, Plot > 8)
noGermSub2$Plot <- as.factor(noGermSub2$Plot)
noGermSub2$Sub_plot <- as.factor(noGermSub2$Sub_plot)

pdf("Analyses/figures/GermNoPart2.pdf", width = 10, height = 8) # or 10
 p <- ggplot(data = noGermSub2, aes(x= Year, y= Germ)) + geom_line(aes(colour=Sub_plot)) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA))+
   labs (y = "No Germinants",
         col = "Sub plot")
 p + facet_grid(vars(Plot), vars(Sub_plot)
                ) + theme(panel.spacing.x = unit(2, "lines"))
dev.off()
# Use vars() to supply variables from the dataset:

pdf("Analyses/figures/HistGermNo.pdf", width = 3, height = 3)
hist(noGermSub$Germ, xlab = "Number Germinants per subplot")
dev.off()

# how many plots never had a germinant? How many lost germinants?
change <- dat
change20 <- subset(change, Plot < 15)
change20$plotGerm <- change20$Count_2020 + change20$Count_2021 + change20$Count_2022 + change20$Count_2023
change20$diff <- change20$Count_2023-change20$Count_2020

change21 <- subset(change, Plot > 14)
change21$plotGerm <- change21$Count_2021 + change21$Count_2022 + change21$Count_2023
change21$diff <- change21$Count_2023-change21$Count_2021

change <- rbind(change20, change21)
change$count <- 1


changeGerm <- aggregate(change [c("count")],
          change[c("plotGerm")],
          FUN = sum)

changeDiff <- aggregate(change [c("count")],
                       change[c("diff")],
                       FUN = sum)

barplot(changeGerm$count)

pdf("Analyses/figures/BarplotGermSurvival.pdf", width = 3, height = 3)
ggplot(data = changeGerm[2:5,], aes (x= plotGerm, y = count)) +
  geom_bar(stat ="identity") +
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA)) +
  labs (x= "Number of years survived",
       y= "Number of Germinants")
dev.off()

ggplot(data = changeDiff, aes (x= diff, y = count)) +
  geom_bar(stat ="identity") +
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA))

temp <- subset(dat, Count_2020 == "0" & Count_2021 == "0"& Count_2022 == "0"& Count_2023 == "0")

# what about the plots with MAD or Dead?
mad <- subset(change,  Height_2021 == "MAD" | Height_2022 == "MAD" | Height_2023 == "MAD")

germ2020 <- subset(dat,  Height_2020 > 0)
germ2021 <- subset(dat,  Height_2021 > 0)
germ2022 <- subset(dat,  Height_2022 > 0)
germ2023 <- subset(dat,  Height_2023 > 0)

germNo20 <- nrow(germ2020)
germNo21 <- nrow(germ2021)
germNo22 <- nrow(germ2022)
germNo23 <- nrow(germ2023)

require(plyr)
mad20 <- count(mad, "Height_2021")
mad21 <- count(mad, "Height_2022")
mad22 <- count(mad, "Height_2023")

madDead <- data.frame(year=c("2021","2022","2023"), count = c(mad20[10,2],mad21[8,2],mad22[5,2], 0,2,14), type = c("MAD","MAD","MAD","dead","dead","dead"), percent <- c((mad20[10,2]/germNo20)*100,(mad21[8,2]/germNo21)*100,(mad22[5,2]/germNo20)*100, NA,NA,NA))
names(madDead) <- c("year","count","type","percent")
madDead$percent <- round(madDead$percent, 1)

pdf("Analyses/figures/numberDead.pdf", height = 5, width = 5)
ggplot(data = madDead, aes (x= year, y = count)) +
  geom_bar(stat ="identity", aes (fill = type), position = "dodge") + 
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x = "Year", y = "Count", cex =2)
dev.off()


pdf("Analyses/figures/percentDead.pdf", height = 5, width = 5)
ggplot(data = madDead, aes (x= year, y = percent)) +
  geom_bar(stat ="identity") + 
  geom_text(aes(label=percent), vjust=1.6, color="white", size=3.5) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x = "Year", y = "Percent", cex =2)
dev.off()
