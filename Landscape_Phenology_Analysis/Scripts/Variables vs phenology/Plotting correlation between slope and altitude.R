gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets
phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]
nrow(phenology.gis)


model<-lm(DEM_MEAN~SLOPE_MEAN,phenology.gis)
with(phenology.gis,plot(DEM_MEAN~SLOPE_MEAN))
abline(lm(DEM_MEAN~SLOPE_MEAN,phenology.gis), col="red")
cor(phenology.gis$DEM_MEAN,phenology.gis$SLOPE_MEAN)
cor.test(phenology.gis$DEM_MEAN,phenology.gis$SLOPE_MEAN)
