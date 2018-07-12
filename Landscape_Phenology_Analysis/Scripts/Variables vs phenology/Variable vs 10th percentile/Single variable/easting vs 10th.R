gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets

nrow(phenology.gis)


############################ allocating values ####################################


par(mfrow = c(1, 1))

summary(phenology.gis$POINT_X)
phenology.gis$POINT_X.label[phenology.gis$POINT_X<(252700)]<-"Western"  #using mean
phenology.gis$POINT_X.label[phenology.gis$POINT_X>(252700)]<-"Eastern"  
table(phenology.gis$POINT_X.label)


library(lme4)
model<-lmer(daynum.tenth~phenology.gis$POINT_X.label+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.tenth~phenology.gis$POINT_X.label))


############################## Testing models #############################

modelnull<-lmer(daynum.tenth~year+(1|site),phenology.gis)
anova(model,modelnull)
library(plyr)
test<-ddply(phenology.gis, c("POINT_X.label"),summarise,
            N=length(daynum.tenth),
            mean=mean(daynum.tenth),
            sd=sd(daynum.tenth),
            se=sd/ sqrt(N),
            min = min(daynum.tenth),
            max=max(daynum.tenth))
head(test)