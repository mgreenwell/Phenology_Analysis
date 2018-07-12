

###########################################################
Fitted_Values<-read.csv("Landscape_Phenology_Analysis/Data/GAM_Data/Meadow_Brown_Fitted_Values.txt",header=T)  
names(Fitted_Values)<-tolower(names(Fitted_Values))
head(Fitted_Values)
names(Fitted_Values)
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
nrow(gis.data)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
nrow(gis.data)
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
nrow(gis.data)
gis.data$LCper<-gis.data$LC/(pi*500^2)*100  


site.list.2003<-unique(Fitted_Values$transect_code[Fitted_Values$year==2003])
length(site.list.2003)

site.list.2011<-unique(Fitted_Values$transect_code[Fitted_Values$year==2011])
length(site.list.2011)

site.list.2003.2011<-site.list.2003[site.list.2003%in%site.list.2011]

length(site.list.2003.2011)  

#tail(with(Fitted_Values,table(transect_code,year)))
#head(with(Fitted_Values,table(transect_code,year)))
#summary(gis.data$LCper)
gis.data$LC.label[gis.data$LCper<(3.108)]<-"low lc"  
gis.data$LC.label[gis.data$LCper>(3.108)]<-"high lc"  

gis.data<-gis.data[(gis.data$SLOPE_MEAN >=0),]#remove values below 0
#summary(gis.data$SLOPE_MEAN)
gis.data$SLOPE_MEAN.label[gis.data$SLOPE_MEAN<(5.152)]<-"shallow slope"  
gis.data$SLOPE_MEAN.label[gis.data$SLOPE_MEAN>(5.152)]<-"steep slope"  

for (i in 1:nrow(gis.data)){
  gis.data$LC.SLOPE[i]<-paste(gis.data[i,"LC.label"],gis.data[i,"SLOPE_MEAN.label"],sep=".")
}
nrow(gis.data[gis.data$LC.SLOPE=="high lc.steep slope",])
table(gis.data$LC.SLOPE)

gis.data$siteno.gref <- as.numeric(as.character(gis.data$siteno.gref))
gis.data<-gis.data[order(gis.data$siteno.gref),]


site.list<-unique(gis.data[gis.data$LC.SLOPE=="high lc.steep slope",1])
site.list
length(site.list)
site.list<-site.list[site.list%in%site.list.2003.2011]
length(site.list)
site.list
site.list<-as.numeric((as.character(site.list)))
site.list<-site.list[order(site.list)]

site.list
####################################################################################################


phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/Meadow_Brown_Phenology.csv",header=T)
phenology<-phenology[c(1,2,6)]
head(phenology)
names(phenology)
summary(phenology$year)
nrow(phenology)

phenology.year <- phenology[phenology$year %in% c(2003, 2011), ]
head(phenology.year)
phenology.year

nrow(phenology.year)

site.list.year<-phenology.year[phenology.year$site%in%site.list,]
site.list.year


with(site.list.year, boxplot(daynum.mean~year,xlab="Year",ylab="Flight Period Mean Day"))

 library(plyr)
test<-ddply(site.list.year, c("year"),summarise,
            N=length(daynum.mean),
            mean=mean(daynum.mean),
            sd=sd(daynum.mean),
            se=sd/ sqrt(N),
            min = min(daynum.mean),
            max=max(daynum.mean))
head(test)



t.test(site.list.year$daynum.mean~site.list.year$year,paired=TRUE)
     
