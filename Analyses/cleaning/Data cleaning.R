####script to create light data for Ph.D. 
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/LI-COR Data (Manning Park)")

library(tidyverse)
library(dbplyr)
library(data.table)
library(readr)
library(reshape2)

#### uploads all the data in the LI-COR Data folder
tbl <-
  list.files(pattern = "*.xls") %>% 
  map_df(~read.delim(.))

### Removing unwanted rows from dataset
tbl<- tbl[tbl$Model.Name!="Serial Number",]
tbl<- tbl[tbl$Model.Name!="Memo",]

### Rename column names
names(tbl)[names(tbl) == "Model.Name"] <- "Variable"
names(tbl)[names(tbl) == "LI.180"] <- "Value"

write_csv(tbl, "licordata2.csv")

### add species and site data in excel (figure out a better way to do this!)

### Adding time data! 
# creates a table with just time data
tbl3 <- filter(tbl2,tbl2$Variable == "Time" )

#renames Value column as time
names(tbl3)[names(tbl3)== "Value"] <- "time"

#creates character vector  with repeating times
ASDAD <-rep(tbl3$time, 442)

#Makes character vector into a dataframe 
df<- as.data.frame(ASDAD)

# rearranges time in ascending order
df<- arrange(df, ASDAD)

#Makes character vector into a dataframe 
df<- as.data.frame(ASDAD)

#merges previous lightcor data set with a new time data
lightdata<- cbind(licordata, df)

#changes name of column to time
names(lightdata)[names(lightdata)== "df"] <- "time"

#makes the dataset wide instead of long
lightdatawide<- dcast(lightdata, Species+Site+time~Variable, value.var = "Value", na.rm=TRUE)

#breaks time varible into date and time
breakbywhitespacetime <- strsplit(as.character(lightdatawide$time), "_", fixed=TRUE) 

#creates a new column with time of measurement
lightdatawide$Time <- unlist(lapply(breakbywhitespacetime, function(x) x[2]))

#further breaks time column into day, month and year
breakbywhitespacetime2 <- strsplit(as.character(lightdatawide$time), "/", fixed=TRUE)

lightdatawide$year <- unlist(lapply(breakbywhitespacetime2, function(x) x[1]))
lightdatawide$month <- unlist(lapply(breakbywhitespacetime2, function(x) x[2]))
lightdatawide$day <-unlist(lapply(breakbywhitespacetime2, function(x) x[3]))

#removes everything after a _ to retain just the day of measurement 
lightdatawide$day <- gsub("_.*","",lightdatawide$day)

#creates a character vector with weather  
weather <- rep("cloudy", 766)

#merges previous lightcor data set with weather column
lightdatawide<- cbind(lightdatawide, weather)

#splits species column into species latin binomial ("^([[:alpha:]]*).*",
lightdatawide$specieslatbi<-sub("^([[:alpha:]]*).*", "\\1", lightdatawide$Species)

#splits species column into only IDs
lightdatawide$ID<- as.numeric(str_extract_all(lightdatawide$Species, "[0-9]+"))

#loop that returns either "a" for adult or "s' for seedling
#for every row in lightdatawide$Species
for (i in 1:length(lightdatawide$Species)){
  #if species name starts with an "S" then this puts an "s" in the lightdatawide$seedlingoradult column
if (substring(lightdatawide$Species [i], 1, 1)=="S"){
  lightdatawide$seedlingoradult[i] <- "s"
} else {
  #if species name doesn't starts with an "S" then this puts an "a" in the lightdatawide$seedlingoradult column
  lightdatawide$seedlingoradult[i] <- "a"
}
}

#removing Time column
lightdatawide<- lightdatawide[,-3] 

write_csv(light, "LiCordatafinal.csv")


