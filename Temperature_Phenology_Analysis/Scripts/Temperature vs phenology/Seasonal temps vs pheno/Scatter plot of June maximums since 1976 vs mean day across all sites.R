library(ggplot2)
library(plyr)
library(reshape)
phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)
temperature<-read.csv("Temperature_Phenology_Analysis/Data/monthly.maximums.csv",header=T)

mean.means<-aggregate(phenology[, 6], list(phenology$year), mean)
mean.means<-rename(mean.means,c("Group.1"="year","x"="average.mean"))
head(mean.means)

temperature<-temperature[!temperature$year < "1976", ]
temperature

phenology.temperature<-merge(mean.means,temperature, by.x="year")
head(phenology.temperature)


mean.temp<-ggplot(phenology.temperature, aes(x=jun, y=average.mean)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, col="black")+ylab("Mean Flight Day")+xlab("June Maximum Temperature ?C") + theme_bw() +
  theme(axis.title = element_text (,,,15)) +
  theme(axis.text = element_text(,,,12)) 
mean.temp

mean.temp<-lm(average.mean~jun, data=phenology.temperature)
summary(mean.temp)
