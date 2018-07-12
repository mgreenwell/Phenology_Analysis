library(ggplot2)
library(reshape)
library(plyr)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)
temperature<-read.csv("Temperature_Phenology_Analysis/Data/seasonal.HADCET.mean.temps.csv",header=T)
#head(phenology)
#head(temperature)
########################sorting data frames into working formats##########
#forming three dataframes
#phenology.temperature                  dataframe for quadplot
#phenology.temperature.fourway          dataframe for individual graphs in quadplot
#phenology.temperature.individuals      dataframe for individual plots and lms



tenth.means<-aggregate(phenology[, 3], list(phenology$year), mean)
head(tenth.means)
tenth.means<-rename(tenth.means,c("Group.1"="year","x"="average.tenth"))

nintieth.means<-aggregate(phenology[, 5], list(phenology$year), mean)
nintieth.means<-rename(nintieth.means,c("Group.1"="year","x"="average.nintieth"))

mean.means<-aggregate(phenology[, 6], list(phenology$year), mean)
mean.means<-rename(mean.means,c("Group.1"="year","x"="average.mean"))


range.means<-aggregate(phenology[, 7], list(phenology$year), mean)
range.means<-rename(range.means,c("Group.1"="year","x"="average.range"))


tenth.mean<-merge(tenth.means,mean.means, by.x="year")
nintieth.range<-merge(nintieth.means, range.means, by.x="year")
phenology.means<-merge(tenth.mean,nintieth.range, by.x="year")

phenology.means.melt <- melt(phenology.means, id=(c("year")))
phenology.means.melt<-rename(phenology.means.melt,c("variable"="flight.period","value"="length.date"))
#head(phenology.means.melt)
#phenology.means

#temperature
temperature<-temperature[!temperature$year < "1976", ]
temperature.melt<-melt(temperature, id=(c("year")))
#temperature.melt
temperature.melt<-rename(temperature.melt,c("variable"="season","value"="temperature"))
temperature.melt$season<- revalue(temperature.melt$season, c("MAM"="Spring (MAM)","JJA"="Summer (JJA)","SON"="Autumn (SON)","DJF"="Winter (DJF)"))
head(temperature.melt)



phenology.temperature.individuals<-merge(phenology.means,temperature, by.x="year")
head(phenology.temperature.individuals)
phenology.temperature.individuals<-mutate(phenology.temperature.individuals, DIF=JJA-MAM)
head(phenology.temperature.individuals)

DIF.range<-ggplot(phenology.temperature.individuals, 
                  aes(x = DIF, y = average.range)) +
geom_point(shape=1) + 
geom_smooth(method = lm, col = "black") + 
  theme_bw() +
  theme(axis.title = element_text (,,,15)) +
  theme(axis.text = element_text(,,,12)) +
  ylab("Flight Period Length (Days)") + 
  xlab("Spring Summer Temperature Differential/?C")
DIF.range

DIF.range<-lm(average.range~DIF, data=phenology.temperature.individuals)
summary(DIF.range)
