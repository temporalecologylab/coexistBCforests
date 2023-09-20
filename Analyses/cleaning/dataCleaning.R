#Started Sept 19 2023 by D Loughnan

# aim of this code is to:
# 1. explore and clean the data from several years of seedling survival observations
# 2. Make some plots to help us decide what our next steps should be.
rm(list=ls()) 

require(reshape2)
setwd("~/Documents/github/Coexistence-in-BC-Forests")

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


plot(noGerm$variable, noGerm$value, col = as.factor(noGerm$Plot))

noGerm$Plot <- as.factor(noGerm$Plot)

pdf("Analyses/figures/noGermTime.pdf", width = 5, height = 5)
ggplot(data = noGerm, aes(x=variable, y=value)) + geom_line(aes(colour=Plot))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Year",
       y = "No Germinants")
dev.off()

# Site 6 and 14 have 18 germinates in 2020!

# Now let's look into the sub plots!
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

p <- ggplot(data = noGermSub1, aes(x= Year, y= Germ)) + geom_line(aes(colour=Sub_plot)) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA))
 p + facet_grid(vars(Plot), vars(Sub_plot))
 
noGermSub2 <- subset(noGermSub, Plot > 8)
noGermSub2$Plot <- as.factor(noGermSub2$Plot)
noGermSub2$Sub_plot <- as.factor(noGermSub2$Sub_plot)
 
 p <- ggplot(data = noGermSub2, aes(x= Year, y= Germ)) + geom_line(aes(colour=Sub_plot)) + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA))
 p + facet_grid(vars(Plot), vars(Sub_plot))

# Use vars() to supply variables from the dataset:
