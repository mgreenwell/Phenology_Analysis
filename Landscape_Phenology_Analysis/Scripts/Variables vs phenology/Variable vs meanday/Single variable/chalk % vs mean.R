library(lme4)
library(tidyverse)
library(plyr)
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
## Filter data set to include:
## only ukbms rows
gis.data<-gis.data[gis.data$Surv=="UKBMS",]   
## only 500m buffer rows
gis.data<-gis.data[gis.data$buffer=="500",]   
## Add row - convert LC to percentage of LC
gis.data$LCper<-gis.data$LC/(pi*500^2)*100           
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)
head(phenology)

#merge datasets
phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") 
#nrow(phenology.gis)

############################ allocating values ####################################


#hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,6000),xlim =c(0,100), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")
summary(phenology.gis$LCper)
## Values above mean are high, values below mean are low
phenology.gis$LC.label[phenology.gis$LCper<(4.1273)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(4.1273)]<-"high lc"  
table(phenology.gis$LC.label)


## Run mixed effects model for mean vs chalk and year as fixed effects
# Site as a random effect
# (1|site) gives site as random effect- not a factor of interest
model <- lmer(daynum.mean ~ phenology.gis$LC.label + 
                year + 
                (1|site), phenology.gis)

summary(model)

with(phenology.gis, boxplot(daynum.mean~phenology.gis$LC.label))


############################## Testing models #############################

## Create null model removing chalk from analysis
modelnull<-lmer(daynum.mean~year+(1|site),phenology.gis)

## Compare mmodels to see if significantly different. If not then models can be 
#treated as the same therefore chalk has no effect on mean.
## If different then  model with lowest AIC is best fit.
anova(model,modelnull)
## Model with chalk is better fit

## Find values for means of groups
test<-ddply(phenology.gis, c("LC.label"),summarise,
            N=length(daynum.mean),
            mean=mean(daynum.mean),
            sd=sd(daynum.mean),
            se=sd/ sqrt(N),
            min = min(daynum.mean),
            max=max(daynum.mean))
head(test)

