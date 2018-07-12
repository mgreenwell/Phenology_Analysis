library(tidyverse)
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets

summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(4.1273)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(4.1273)]<-"high lc"  
table(phenology.gis$LC.label)

summary(phenology.gis$DEM_MEAN)
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN<(85.76)]<-"low altitude"  #using mean
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN>(85.76)]<-"high altitude"  
table(phenology.gis$DEM_MEAN.label)


phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]#remove values below 0
summary(phenology.gis$SLOPE_MEAN)
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(phenology.gis$SLOPE_MEAN.label)

for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.SLOPE[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}
table(phenology.gis$LC.SLOPE)
head(phenology.gis)
phenology.gis<-phenology.gis[phenology.gis$LC.SLOPE=="high lc.steep slope",]
head(phenology.gis)
summary(phenology.gis$LC.SLOPE)

phenology.gis<-phenology.gis[c(1:7)]
head(phenology.gis)

temperature<-read.csv("Temperature_Phenology_Analysis/Data/seasonal.HADCET.mean.temps.csv",header=T)
temperature<-temperature[!temperature$year < "1976", ]
head(temperature)
temperature$AV = apply(temperature[,c(2:5)],1,mean)
head(temperature)

head(phenology.gis)
phenology.temperature<-merge(phenology.gis,temperature, by.x="year",by.y="year")
head(phenology.temperature)
phenology.temperature




plot<-ggplot(phenology.temperature, aes(x=AV, y=daynum.range)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("Yearly mean average temperature vs phenology")+theme(legend.title=element_blank())
plot

year.range<-lm(AV~daynum.range, data=phenology.temperature)
summary(year.range)

plot1<-ggplot(phenology.temperature, aes(x=DJF, y=daynum.range)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("temperature vs range")+theme(legend.title=element_blank())

year.range<-lm(DJF~daynum.range, data=phenology.temperature)
summary(year.range)

plot2<-ggplot(phenology.temperature, aes(x=MAM, y=daynum.range)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("temperature vs range")+theme(legend.title=element_blank())

year.range<-lm(MAM~daynum.range, data=phenology.temperature)
summary(year.range)

plot3<-ggplot(phenology.temperature, aes(x=JJA, y=daynum.range)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("temperature vs range")+theme(legend.title=element_blank())

year.range<-lm(JJA~daynum.range, data=phenology.temperature)
summary(year.range)

plot4<-ggplot(phenology.temperature, aes(x=SON, y=daynum.range)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("temperature vs range")+theme(legend.title=element_blank())

year.range<-lm(SON~daynum.range, data=phenology.temperature)
summary(year.range)

library(gridExtra)
grid.arrange(plot1, plot2,plot3,plot4, nrow=2, ncol=2)

plot3


