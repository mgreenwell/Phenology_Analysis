library(tidyverse)

search()
ls()
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
head(gis.data)
summary(gis.data$LC)
summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets

nrow(phenology.gis)


############################ allocating values ####################################


par(mfrow = c(1, 1))


phenology.gis$LC.label[phenology.gis$LCper<(3.108)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(3.108)]<-"high lc"  

phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN<(85.76)]<-"low altitude"  #using mean
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN>(85.76)]<-"high altitude"  

phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]#remove values below 0

phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  

for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.SLOPE[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}

head(phenology.gis)


boxplot <- ggplot(phenology.gis, 
                  aes(x = LC.SLOPE, y = daynum.mean)) +
  geom_boxplot() +
  ylab("Mean Flight Day") +
  xlab("Slope Angle and Chalk Percentage") +
scale_x_discrete(labels=c("high lc.shallow slope" = "Shallow Slope\nHigh Chalk Percentage", 
                          "high lc.steep slope" = "Steep Slope\nHigh Chalk Percentage",
                          "low lc.shallow slope" = "Shallow Slope\nLow Chalk Percentage", 
                          "low lc.steep slope" = "Steep Slope\nLow Chalk Percentage")) + 
  annotate("text", x = 1, y = 160, label = "A") + 
  annotate("text", x = 2, y = 160, label = "B") + 
  annotate("text", x = 3, y = 160, label = "C") +
  annotate("text", x = 4, y = 160, label = "A")
  
boxplot

