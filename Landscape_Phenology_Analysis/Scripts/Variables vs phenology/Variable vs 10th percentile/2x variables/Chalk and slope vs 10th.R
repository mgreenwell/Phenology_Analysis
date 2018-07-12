library(tidyverse)
library(lme4)
library(plyr)

gis.data <-
  read.table(
    "Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",
    header = T)
# Sort data so that only includes
#only ukbms rows
gis.data <- gis.data[gis.data$Surv == "UKBMS", ]
#only 500 buffer rows
gis.data <- gis.data[gis.data$buffer == "500", ]   
#Add row - convert LC to percentage of LC
gis.data$LCper <-gis.data$LC / (pi * 500 ^ 2) * 100           


phenology <-
  read.csv(
    "Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",
    header = T)
# limits to univoltine species, records where flight period is greater than zero
#i.e. the species was recorded more than once in the year at that site, and 
#species occupy 10 or more  10km squares within the UKBMS (see email from Marc 
#Botham  Wed 23/10/2013 14:33)

#merge datasets
phenology.gis <- merge(phenology, gis.data, by.x = "site", by.y = "siteno.gref") 

#nrow(phenology.gis)


############################ allocating values ###################################

## Chalk

#hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,6000),xlim =c(0,100), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")
summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(4.1273)]<-"Non Chalk"  
phenology.gis$LC.label[phenology.gis$LCper>(4.1273)]<-"Chalk"  
table(phenology.gis$LC.label)

## Slope

#hist(phenology.gis$SLOPE_MEAN,breaks=40,col="red",ylim =c(0,700),xlim =c(0,25), xlab = "Mean slope, degrees from horizontal",main="Histogram of mean slope angle")

#remove values below 0
phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]
#nrow(phenology.gis)

summary(phenology.gis$SLOPE_MEAN)
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(phenology.gis$SLOPE_MEAN.label)


############################ assigning labels #################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.SLOPE[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}
table(phenology.gis$LC.SLOPE)

model<-lmer(daynum.tenth~LC.SLOPE+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
#with(phenology.gis, boxplot(daynum.tenth~LC.SLOPE))


############################## Testing models #############################


modelnull<-lmer(daynum.tenth~year+(1|site),phenology.gis)
anova(model,modelnull)



bargraph<-ddply(phenology.gis, c("LC.SLOPE"),summarise,
                N=length(daynum.tenth),
                mean=mean(daynum.tenth),
                sd=sd(daynum.tenth),
                se=sd/ sqrt(N))
head(bargraph)

#### Deletion tests

phenology.gis$LC.SLOPE2<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE2<-as.factor(phenology.gis$LC.SLOPE2)
levels(phenology.gis$LC.SLOPE2)
levels(phenology.gis$LC.SLOPE2)<-c("A","B","C","A")
levels(phenology.gis$LC.SLOPE2)
model2<-lmer(daynum.tenth~LC.SLOPE2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$LC.SLOPE3<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE3<-as.factor(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)<-c("A","B","A","C")
levels(phenology.gis$LC.SLOPE3)
model3<-lmer(daynum.tenth~LC.SLOPE3+year+(1|site),phenology.gis)
anova(model2,model3)

#######

phenology.gis$LC.SLOPE4<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE4<-as.factor(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)<-c("A","A","B","C")
levels(phenology.gis$LC.SLOPE4)
model4<-lmer(daynum.tenth~LC.SLOPE4+year+(1|site),phenology.gis)
anova(model2,model4)

#######

phenology.gis$LC.SLOPE5<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE5<-as.factor(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)<-c("B","C","A","A")
levels(phenology.gis$LC.SLOPE5)
model5<-lmer(daynum.tenth~LC.SLOPE5+year+(1|site),phenology.gis)
anova(model2,model5)

#######

phenology.gis$LC.SLOPE6<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE6<-as.factor(phenology.gis$LC.SLOPE6)
levels(phenology.gis$LC.SLOPE6)
levels(phenology.gis$LC.SLOPE6)<-c("B","A","C","A")
levels(phenology.gis$LC.SLOPE6)
model6<-lmer(daynum.tenth~LC.SLOPE6+year+(1|site),phenology.gis)
anova(model2,model6)

#######

phenology.gis$LC.SLOPE7<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE7<-as.factor(phenology.gis$LC.SLOPE7)
levels(phenology.gis$LC.SLOPE7)
levels(phenology.gis$LC.SLOPE7)<-c("B","A","A","C")
levels(phenology.gis$LC.SLOPE7)
model7<-lmer(daynum.tenth~LC.SLOPE7+year+(1|site),phenology.gis)
anova(model2,model7)

#######

phenology.gis$LC.SLOPE8<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE8<-as.factor(phenology.gis$LC.SLOPE8)
levels(phenology.gis$LC.SLOPE8)
levels(phenology.gis$LC.SLOPE8)<-c("A","A","B","B")
levels(phenology.gis$LC.SLOPE8)
model8<-lmer(daynum.tenth~LC.SLOPE8+year+(1|site),phenology.gis)
anova(model2,model8)

#######

phenology.gis$LC.SLOPE9<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE9<-as.factor(phenology.gis$LC.SLOPE9)
levels(phenology.gis$LC.SLOPE9)
levels(phenology.gis$LC.SLOPE9)<-c("A","B","A","B")
levels(phenology.gis$LC.SLOPE9)
model9<-lmer(daynum.tenth~LC.SLOPE9+year+(1|site),phenology.gis)
anova(model2,model9)

#######

phenology.gis$LC.SLOPE10<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE10<-as.factor(phenology.gis$LC.SLOPE10)
levels(phenology.gis$LC.SLOPE10)
levels(phenology.gis$LC.SLOPE10)<-c("A","B","B","A")
levels(phenology.gis$LC.SLOPE10)
model10<-lmer(daynum.tenth~LC.SLOPE10+year+(1|site),phenology.gis)
anova(model2,model10)


#######

phenology.gis$LC.SLOPE11<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE11<-as.factor(phenology.gis$LC.SLOPE11)
levels(phenology.gis$LC.SLOPE11)
levels(phenology.gis$LC.SLOPE11)<-c("A","A","A","B")
levels(phenology.gis$LC.SLOPE11)
model11<-lmer(daynum.tenth~LC.SLOPE11+year+(1|site),phenology.gis)
anova(model2,model11)

#######

phenology.gis$LC.SLOPE12<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE12<-as.factor(phenology.gis$LC.SLOPE12)
levels(phenology.gis$LC.SLOPE12)
levels(phenology.gis$LC.SLOPE12)<-c("A","A","B","A")
levels(phenology.gis$LC.SLOPE12)
model12<-lmer(daynum.tenth~LC.SLOPE12+year+(1|site),phenology.gis)
anova(model2,model12)

#######

phenology.gis$LC.SLOPE13<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE13<-as.factor(phenology.gis$LC.SLOPE13)
levels(phenology.gis$LC.SLOPE13)
levels(phenology.gis$LC.SLOPE13)<-c("A","B","A","A")
levels(phenology.gis$LC.SLOPE13)
model13<-lmer(daynum.tenth~LC.SLOPE13+year+(1|site),phenology.gis)
anova(model2,model13)

#######

phenology.gis$LC.SLOPE14<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE14<-as.factor(phenology.gis$LC.SLOPE14)
levels(phenology.gis$LC.SLOPE14)
levels(phenology.gis$LC.SLOPE14)<-c("B","A","A","A")
levels(phenology.gis$LC.SLOPE14)
model14<-lmer(daynum.tenth~LC.SLOPE14+year+(1|site),phenology.gis)
anova(model2,model14)

############################### plotting model2 ###############################

# Simplified boxplot with three groups
#with(phenology.gis, boxplot(daynum.tenth ~ LC.SLOPE2, xlab = "Site type",
#ylab = "flight period meanday", ylim = c(0, 200)))

## Four bars but with post hoc labels

boxplot <- ggplot(phenology.gis,
                  aes(x = LC.SLOPE, y = daynum.tenth)) +
  geom_boxplot() +
  ylab("Flight Start Day") +
  theme_bw() +
  theme(axis.title = element_text (,,,15)) +
  theme(axis.text = element_text(,,,12)) +
  xlab("Slope Angle and Chalk Percentage") + 
  annotate("text", x = "Chalk.shallow slope", y = 160, label = "A") +
  annotate("text", x = "Chalk.steep slope", y = 160, label = "B") +
  annotate("text", x = "Non Chalk.shallow slope", y = 160, label = "C") +
  annotate("text", x = "Non Chalk.steep slope", y = 160, label = "A") +
  scale_x_discrete(
    labels = c(
      "Chalk.shallow slope" = "Shallow Slope\nChalk",
      "Chalk.steep slope" = "Steep Slope\nChalk",
      "Non Chalk.shallow slope" = "Shallow Slope\nNon Chalk",
      "Non Chalk.steep slope" = "Steep Slope\nNon Chalk"
    ))
boxplot
?element_text
test <- ddply(
  phenology.gis,
  c("LC.SLOPE2"),
  summarise,
  N = length(daynum.tenth),
  mean = mean(daynum.tenth),
  sd = sd(daynum.tenth),
  se = sd / sqrt(N),
  min = min(daynum.tenth),
  max = max(daynum.tenth)
)
head(test)
