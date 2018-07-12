library(tidyverse)
library(ggfortify)

gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data <- gis.data[gis.data$Surv == "UKBMS", ]    #only ukbms rows
gis.data <- gis.data[gis.data$buffer == "500", ]   #only 500 buffer rows
gis.data$LCper <- gis.data$LC / (pi * 500 ^ 2) * 100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


#merge datasets
phenology_gis <- merge(phenology, gis.data, by.x = "site", by.y = "siteno.gref")

head(phenology_gis)
summary(phenology_gis$POINT_X)


#plot scatter plot of lattitude vs flight start date with line of best fit and 95% confidence intervals
start_vs_northing <- ggplot(phenology_gis, aes(x=POINT_X, y=daynum.tenth)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  xlab("Site northing in KM") + 
  ylab("Flight period start day") +
  ggtitle("lattitude vs start day")


#plot scatter plot of lattitude vs flight end date with line of best fit and 95% confidence intervals
end_vs_northing <- ggplot(phenology_gis, aes(x=POINT_X, y=daynum.nintieth)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  xlab("Site northing in KM") + 
  ylab("Flight period end day") +
  ggtitle("lattitude vs end day")


#plot scatter plot of lattitude vs flight mean date with line of best fit and 95% confidence intervals
mean_vs_northing <- ggplot(phenology_gis, aes(x=POINT_X, y=daynum.mean)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  xlab("Site northing in KM") + 
  ylab("Flight period mean day") +
  ggtitle("lattitude vs mean day")


#plot scatter plot of lattitude vs flight range with line of best fit and 95% confidence intervals
range_vs_northing <- ggplot(phenology_gis, aes(x=POINT_X, y=daynum.range)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  xlab("Site northing in KM") + 
  ylab("Flight period range") +
  ggtitle("lattitude vs range")

library(gridExtra)
grid.arrange(start_vs_northing, end_vs_northing, mean_vs_northing, range_vs_northing)





####modeling graphs 
#start
model_start<-lm(POINT_X ~ daynum.tenth, data = phenology_gis)
#check assumptions
autoplot(model_start)
#results on linear model
anova(model_start)
summary(model_start)

#end
model_end<-lm(POINT_X ~ daynum.nintieth, data = phenology_gis)
#check assumptions
autoplot(model_end)
#results on linear model
anova(model_end)
summary(model_end)

#mean
model_mean<-lm(POINT_X ~ daynum.mean, data = phenology_gis)
#check assumptions
autoplot(model_mean)
#results on linear model
anova(model_mean)
summary(model_mean)

#range
model_range<-lm(POINT_X ~ daynum.range, data = phenology_gis)
#check assumptions
autoplot(model_range)
#results on linear model
anova(model_range)
summary(model_range)




#plot point x vs point y. creates very crude map of the uk by plot points
ggplot(phenology_gis, aes(x = POINT_X, y = POINT_Y)) + geom_point()


########



#set points defined by mean point x values
phenology_gis$POINT_X.label[phenology_gis$POINT_X<(448100)]<-"Western"  #using mean
phenology_gis$POINT_X.label[phenology_gis$POINT_X>(448100)]<-"Eastern"  
table(phenology_gis$POINT_X.label)

#set points defined by mean point y value
summary(phenology_gis$POINT_Y)
phenology_gis$POINT_Y.label[phenology_gis$POINT_Y<(252727)]<-"Southern"  #using mean
phenology_gis$POINT_Y.label[phenology_gis$POINT_Y>(252727)]<-"Northern"  
table(phenology_gis$POINT_Y.label)

phenology_gis$POINT_XY.label[phenology_gis$POINT_X<(448100) & phenology_gis$POINT_Y<(252727)]<-"South West"
phenology_gis$POINT_XY.label[phenology_gis$POINT_X>(448100) & phenology_gis$POINT_Y<(252727)]<-"South East"
phenology_gis$POINT_XY.label[phenology_gis$POINT_X<(448100) & phenology_gis$POINT_Y>(252727)]<-"North West"
phenology_gis$POINT_XY.label[phenology_gis$POINT_X>(448100) & phenology_gis$POINT_Y>(252727)]<-"North East"
table(phenology_gis$POINT_XY.label)
#set points defined by mean point y value
phenology_gis %<% mutate(POINT_XY = )



#plot map separating north and south points by colour
map<-ggplot(phenology_gis, aes(x = POINT_X, y = POINT_Y, colour = POINT_Y.label, shape = POINT_X.label)) + geom_point(size = 1) + scale_color_manual(values=c("black", "red"))
map

boxplot1<-ggplot(phenology_gis, aes(x = POINT_Y.label, y = daynum.tenth)) + geom_boxplot()
boxplot1

######
#need to work out how to find the mean of northern and southern. Could probably do using plyr but need to find function in dplyr otherwise will start to bugger up things.
######

phenology_gis %>%
  group_by(POINT_Y.label) %>%
  summarise(meanY = mean(daynum.tenth))

library(plyr)
test<-ddply(phenology_gis, c("POINT_Y.label"),summarise,
            N=length(daynum.tenth),
            mean=mean(daynum.tenth),
            sd=sd(daynum.tenth),
            se=sd/ sqrt(N),
            min = min(daynum.tenth),
            max=max(daynum.tenth))
head(test)

model2 <- lm(daynum.tenth ~ POINT_Y.label, data = phenology_gis) 
autoplot(model2)


anova(model2)
summary(model2)
