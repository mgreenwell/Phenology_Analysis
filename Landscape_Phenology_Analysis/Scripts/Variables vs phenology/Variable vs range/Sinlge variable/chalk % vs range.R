
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

#hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,6000),xlim =c(0,100), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")
summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(3.108)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(3.108)]<-"high lc"  
table(phenology.gis$LC.label)


library(lme4)
model<-lmer(daynum.range~phenology.gis$LC.label+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.range~phenology.gis$LC.label))


############################## Testing models #############################

modelnull<-lmer(daynum.range~year+(1|site),phenology.gis)
anova(model,modelnull)
library(plyr)
test<-ddply(phenology.gis, c("LC.label"),summarise,
            N=length(daynum.range),
            mean=mean(daynum.range),
            sd=sd(daynum.range),
            se=sd/ sqrt(N),
            min = min(daynum.range),
            max=max(daynum.range))
head(test)
