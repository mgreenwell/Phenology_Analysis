library(tidyverse)
library(gridExtra)

gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge 

############################ allocating values #################################


phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]#remove values below 0
summary(phenology.gis$SLOPE_MEAN)
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(phenology.gis$SLOPE_MEAN.label)


## Start date
start.date <- ggplot(phenology.gis, aes(SLOPE_MEAN.label, daynum.tenth)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Start Day") +
  xlab("Site Type") + 
  theme_bw() 


start.date

## End date
end.date <- ggplot(phenology.gis, aes(SLOPE_MEAN.label, daynum.nintieth)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period End Day") +
  xlab("Site Type") + 
  theme_bw() 
 

end.date

## Mean date
mean.date <- ggplot(phenology.gis, aes(SLOPE_MEAN.label, daynum.mean)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Mean Day") +
  xlab("Site Type") + 
  theme_bw() 

mean.date

## Range
range.date <- ggplot(phenology.gis, aes(SLOPE_MEAN.label, daynum.range)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Range") +
  xlab("Site Type") + 
  theme_bw()

range.date

grid.arrange(
  start.date,
  end.date,
  mean.date,
  range.date,
  ncol = 2,
  nrow = 2
)

