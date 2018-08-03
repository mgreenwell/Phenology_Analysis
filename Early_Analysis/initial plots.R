phenology<-read.csv("C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\Early_Analysis\\pheno_var_2012_good_sp.txt",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)
names(phenology)<-tolower(names(phenology))
nrow(phenology)

phenology<-phenology[phenology$common_name=="Meadow Brown",]
head(phenology)
nrow(phenology)
summary(phenology$meanday)
model<-lm(meanday~north, phenology)
summary(model)
with(phenology,plot(meanday~north, xlab="Mean flight day", ylab="Northing value",main="Scatter plot of mean flight day vs northing"))

hist(phenology$meanday,main="Histogram of mean flight day of Meadow Brown",xlab="mean flight day",col="red",breaks=40)
model<-lm(meanday~year,phenology)
summary(model)
with(phenology,boxplot(meanday~year,xlab="Year",ylab="mean flight day",main="mean flight day vs year"))
with(phenology,barplot(meanday~year,xlab="Year",ylab="mean flight day",main="mean flight day vs year"))


gis.data<-read.table("C:\\Users\\Matt\\Documents\\PhD\\BMS_Data_Analysis\\BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]
                   
phenology.gis<-merge(phenology,gis.data,by.x="siteno",by.y="siteno.gref")
phenology.gis<-phenology.gis[!phenology.gis$SLOPE_MEAN<(-500),]
names(phenology.gis)
head(phenology.gis)



model<-lm(meanday~SLOPE_MEAN+factor(year),phenology.gis[phenology.gis$buffer==2000,] )
summary(model)
with(phenology.gis[phenology.gis$buffer==2000,],plot(meanday~SLOPE_MEAN,xlab="mean angle of slope from vertical",main="mead day vs slope angle"))

model<-lm(meanday~NORTHNESS_MEAN+factor(year),phenology.gis[phenology.gis$buffer==2000,] )
summary(model)
with(phenology.gis[phenology.gis$buffer==2000,],plot(meanday~NORTHNESS_MEAN,xlab="Site northness (1= due north)",main="meanday vs northness"))

model<-lm(meanday~EASTNESS_MEAN+factor(year),phenology.gis[phenology.gis$buffer==2000,] )
summary(model)
with(phenology.gis[phenology.gis$buffer==2000,],plot(meanday~EASTNESS_MEAN,xlab="Site eastness (1= due east)",main="meanday vs eastness"))
