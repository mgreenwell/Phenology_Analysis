library(tidyverse)
library(reshape)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)
temperature<-read.csv("Temperature_Phenology_Analysis/Data//seasonal.HADCET.mean.temps.csv",header=T)
#head(phenology)
#head(temperature)

########################sorting data frames into working formats##########
#forming three dataframes
#phenology.temperature                  dataframe for quadplot
#phenology.temperature.fourway          dataframe for individual graphs in quadplot
#phenology.temperature.individuals      dataframe for individual plots and lms



tenth.means<-aggregate(phenology[, 3], list(phenology$year), mean)
#tenth.means<-rename(tenth.means,c("Group.1"="year","x"="average.tenth"))
tenth.means <- select(tenth.means, average.tenth = x, year = Group.1)
#tenth.means

nintieth.means<-aggregate(phenology[, 5], list(phenology$year), mean)
#nintieth.means<-rename(nintieth.means,c("Group.1"="year","x"="average.nintieth"))
nintieth.means <- select(nintieth.means, average.nintieth = x, year = Group.1)
#nintieth.means

mean.means<-aggregate(phenology[, 6], list(phenology$year), mean)
#mean.means<-rename(mean.means,c("Group.1"="year","x"="average.mean"))
mean.means <- select(mean.means, average.mean = x, year = Group.1)
#mean.means

range.means<-aggregate(phenology[, 7], list(phenology$year), mean)
#range.means<-rename(range.means,c("Group.1"="year","x"="average.range"))
range.means <- select(range.means, average.range = x, year = Group.1)
#range.means

tenth.mean<-merge(tenth.means,mean.means, by.x="year")
nintieth.range<-merge(nintieth.means, range.means, by.x="year")
phenology.means<-merge(tenth.mean,nintieth.range, by.x="year")
head(phenology.means)

#phenology.means <- phenology.means %>% gather(flight.period, length.date, average.tenth:average.range)
#head(phenology.means)
head(phenology.means.melt)
phenology.means.melt <- melt(phenology.means, id=(c("year")))
head(phenology.means.melt)
phenology.means.melt <- rename(phenology.means.melt,c("variable"="flight.period","value"="length.date"))
#head(phenology.means.melt)
#phenology.means

#temperature
temperature<-temperature[!temperature$year < "1976", ]
temperature.melt<-melt(temperature, id=(c("year")))
head(temperature.melt)
temperature.melt<-rename(temperature.melt,c("variable"="season","value"="temperature"))
temperature.melt$season <- revalue(temperature.melt$season, c("MAM"="Spring (MAM)","JJA"="Summer (JJA)","SON"="Autumn (SON)","DJF"="Winter (DJF)"))
head(temperature.melt)

phenology.temperature<-merge(phenology.means.melt,temperature.melt, by.x="year")
head(phenology.temperature)
phenology.temperature$flight.period<- revalue(phenology.temperature$flight.period, c("average.tenth"="Start day","average.nintieth"="End day","average.mean"="Mean day","average.range"="Flight period length"))
head(phenology.temperature)

phenology.temperature.fourway<-merge(phenology.means.melt,temperature,by.x="year")
head(phenology.temperature.fourway)

phenology.temperature.individuals<-merge(phenology.means,temperature, by.x="year")
head(phenology.temperature.individuals)



############################################plotting graphs#######################




############################################quad plot#######################################
#plots single window with four plots. Each plot has all four phenological aspects

graphs<-ggplot(phenology.temperature, aes(x=temperature, y=length.date, colour=flight.period,shape=flight.period)) +geom_point(shape=16)+ geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+theme(legend.title=element_blank())
graphs<-graphs+facet_wrap(~season,scales='free')
graphs
