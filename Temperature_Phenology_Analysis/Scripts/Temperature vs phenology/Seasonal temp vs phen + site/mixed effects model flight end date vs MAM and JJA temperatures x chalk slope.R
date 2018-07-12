#load required packages
library(tidyverse)
library(ggfortify)
library(lme4)

#read in data
phenology.temperature.gis <- read.csv(
    "Landscape_Phenology_Analysis/Outputs/combined_raw_data.csv",header = T)

#assign values for high and low chalk
phenology.temperature.gis$LC.label[phenology.temperature.gis$LCper<(3.108)]<-"low.chalk"  
phenology.temperature.gis$LC.label[phenology.temperature.gis$LCper>(3.108)]<-"high.chalk"  

#assign values for steep and shallow slope
phenology.temperature.gis$SLOPE_MEAN.label[phenology.temperature.gis$SLOPE_MEAN<(5.03)]<-"shallow.slope"  
phenology.temperature.gis$SLOPE_MEAN.label[phenology.temperature.gis$SLOPE_MEAN>(5.03)]<-"steep.slope"  

#create new column of slope and chalk values
for (i in 1:nrow(phenology.temperature.gis)){
  phenology.temperature.gis$LC.SLOPE[i]<-paste(phenology.temperature.gis[i,"LC.label"],phenology.temperature.gis[i,"SLOPE_MEAN.label"],sep=".")
}

#head(phenology.temperature.gis)

#remove high.chalk.shallow.slope and low.chalk.steep.slope
phenology.temperature.gis <- phenology.temperature.gis [! phenology.temperature.gis $ LC.SLOPE == "high.chalk.shallow.slope", ]
phenology.temperature.gis <- phenology.temperature.gis [! phenology.temperature.gis $ LC.SLOPE == "low.chalk.steep.slope", ]
#phenology.temperature.gis
head(phenology.temperature.gis)  

#run linear model
spring.summer.end <- lm(daynum.nintieth ~ MAM + JJA, data = phenology.temperature.gis)
anova(spring.summer.end)
summary(spring.summer.end)

#test model assumptions
autoplot(spring.summer.end)


#run mixed effects model
model<-lmer(daynum.nintieth ~ MAM + JJA * LC.SLOPE + (1|site), phenology.temperature.gis)
summary(model)

#simplify model, remove slope type
model2<-lmer(daynum.nintieth ~ MAM + JJA + (1|site), phenology.temperature.gis)
summary(model2)

#comapre models
anova(model, model2)

