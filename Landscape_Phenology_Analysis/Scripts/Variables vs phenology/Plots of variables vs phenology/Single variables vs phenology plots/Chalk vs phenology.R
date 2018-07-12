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


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets



############################ allocating values ####################################

summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(4.1273)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(4.1273)]<-"high lc"  
table(phenology.gis$LC.label)

## Start date
start.date <- ggplot(phenology.gis, aes(LC.label, daynum.tenth)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Start Day") +
  xlab("Site Type") + 
  theme_bw() +
  scale_x_discrete(
    labels = c(
      "high lc" = "Chalk Sites",
      "low lc" = "Non-Chalk Sites"
    ))

start.date

## End date
end.date <- ggplot(phenology.gis, aes(LC.label, daynum.nintieth)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period End Day") +
  xlab("Site Type") + 
  theme_bw() +
  scale_x_discrete(
    labels = c(
      "high lc" = "Chalk Sites",
      "low lc" = "Non-Chalk Sites"
    ))

end.date

## Mean date
mean.date <- ggplot(phenology.gis, aes(LC.label, daynum.mean)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Mean Day") +
  xlab("Site Type") + 
  theme_bw() +
  scale_x_discrete(
    labels = c(
      "high lc" = "Chalk Sites",
      "low lc" = "Non-Chalk Sites"
    ))

mean.date

## Range
range.date <- ggplot(phenology.gis, aes(LC.label, daynum.range)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Flight Period Range") +
  xlab("Site Type") + 
  theme_bw() +
  scale_x_discrete(
    labels = c(
      "high lc" = "Chalk Sites",
      "low lc" = "Non-Chalk Sites"
    ))

range.date

grid.arrange(
  start.date,
  end.date,
  mean.date,
  range.date,
  ncol = 2,
  nrow = 2
)
