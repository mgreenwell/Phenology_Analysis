library(tidyverse)
library(ggfortify)
library(gridExtra)

phenology.gis.temperature <- 
  read.csv(
    "Landscape_Phenology_Analysis/Outputs/combined_raw_data.csv",header = T)

### define high and low chalk

summary(phenology.gis.temperature$LCper)
phenology.gis.temperature$LC.label[phenology.gis.temperature$LCper<(3.108)]<-"low lc"  
phenology.gis.temperature$LC.label[phenology.gis.temperature$LCper>(3.108)]<-"high lc"  
table(phenology.gis.temperature$LC.label)


###  define steep and shallow slope

summary(phenology.gis.temperature$SLOPE_MEAN)
phenology.gis.temperature$SLOPE_MEAN.label[phenology.gis.temperature$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis.temperature$SLOPE_MEAN.label[phenology.gis.temperature$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(phenology.gis.temperature$SLOPE_MEAN.label)


### define steep chalk  etc.

for (i in 1:nrow(phenology.gis.temperature)) {
  phenology.gis.temperature$LC.SLOPE[i] <-
    paste(phenology.gis.temperature[i, "LC.label"],
          phenology.gis.temperature[i, "SLOPE_MEAN.label"],
          sep = ".")
}
table(phenology.gis.temperature$LC.SLOPE)
# head(phenology.gis.temperature)


## remove high chalk, low altitude and low chalk, high altitude

phenology.gis.temperature <- phenology.gis.temperature[!phenology.gis.temperature$LC.SLOPE == "low lc.steep slope", ]
phenology.gis.temperature <- phenology.gis.temperature[!phenology.gis.temperature$LC.SLOPE == "high lc.shallow slope", ]
table(phenology.gis.temperature$LC.SLOPE)

####plotting graphs

start_date<-ggplot(phenology.gis.temperature, aes(x=AV, y=daynum.tenth, colour = LC.SLOPE)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day")+xlab("Temperature ?C")+ ggtitle("Yearly mean average temperature vs start date")+theme(legend.title=element_blank())
#start_date

end_date<-ggplot(phenology.gis.temperature, aes(x=AV, y=daynum.nintieth, colour = LC.SLOPE)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day")+xlab("Temperature ?C")+ ggtitle("Yearly mean average temperature vs end date")+theme(legend.title=element_blank())
#end_date

mean_date<-ggplot(phenology.gis.temperature, aes(x=AV, y=daynum.mean, colour = LC.SLOPE)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day")+xlab("Temperature ?C")+ ggtitle("Yearly mean average temperature vs mean date")+theme(legend.title=element_blank())
#mean_date

range_date<-ggplot(phenology.gis.temperature, aes(x=AV, y=daynum.range, colour = LC.SLOPE)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Days")+xlab("Temperature ?C")+ ggtitle("Yearly mean average temperature vs flight period length")+theme(legend.title=element_blank())
#range_date

grid.arrange(start_date, end_date, mean_date, range_date)


### need to model the above graphs
# graphs workign though which is nice

#separating data so that can plot and separately model the high chalk steep sites and low chalk shallow sites.

phenology.gis.temperature <- select(phenology.gis.temperature, year, site, daynum.tenth, daynum.mean, daynum.nintieth, daynum.range, DJF, MAM, JJA, SON, AV, LC.SLOPE)
head(phenology.gis.temperature)

phenology.spread <- phenology.gis.temperature %>% spread(LC.SLOPE, LC.SLOPE)
phenology.spread <- rename(phenology.spread, high.steep = `high lc.steep slope`)
phenology.spread <- rename(phenology.spread, low.shallow = `low lc.shallow slope`)

head(phenology.spread)
tail(phenology.spread)
phenology.high.steep <- select(phenology.spread, year, site, daynum.tenth, daynum.mean, daynum.nintieth, daynum.range, DJF, MAM, JJA, SON, AV, high.steep)
phenology.low.shallow <- select(phenology.spread, year, site, daynum.tenth, daynum.mean, daynum.nintieth, daynum.range, DJF, MAM, JJA, SON, AV, low.shallow)

phenology.high.steep <- na.omit(phenology.high.steep)
phenology.low.shallow <- na.omit(phenology.low.shallow)


head(phenology.high.steep)
head(phenology.low.shallow)


#can now model all graphs easily



####modeling graphs 

#start date

#start high steep
model.start.highsteep <- lm(daynum.tenth ~ AV, data = phenology.high.steep)
#check assumptions
#autoplot(model.start.highsteep)
#results on linear model
anova(model.start.highsteep)
summary(model.start.highsteep)

#start low shallow
model.start.lowshallow <- lm(daynum.tenth ~ AV, data = phenology.low.shallow)
#check assumptions
#autoplot(model.start.lowshallow)
#results on linear model
anova(model.start.lowshallow)
summary(model.start.lowshallow)



#nintieth percentiles

# high steep
model.start.highsteep <- lm(daynum.nintieth ~ AV, data = phenology.high.steep)
#check assumptions
#autoplot(model.start.highsteep)
#results on linear model
anova(model.start.highsteep)
summary(model.start.highsteep)

# low shallow
model.start.lowshallow <- lm(daynum.nintieth ~ AV, data = phenology.low.shallow)
#check assumptions
#autoplot(model.start.lowshallow)
#results on linear model
anova(model.start.lowshallow)
summary(model.start.lowshallow)



#mean flight date

# high steep
model.start.highsteep <- lm(daynum.mean ~ AV, data = phenology.high.steep)
#check assumptions
#autoplot(model.start.highsteep)
#results on linear model
anova(model.start.highsteep)
summary(model.start.highsteep)

# low shallow
model.start.lowshallow <- lm(daynum.mean ~ AV, data = phenology.low.shallow)
#check assumptions
#autoplot(model.start.lowshallow)
#results on linear model
anova(model.start.lowshallow)
summary(model.start.lowshallow)



#range

# high steep
model.start.highsteep <- lm(daynum.range ~ AV, data = phenology.high.steep)
#check assumptions
#autoplot(model.start.highsteep)
#results on linear model
anova(model.start.highsteep)
summary(model.start.highsteep)

# low shallow
model.start.lowshallow <- lm(daynum.range ~ AV, data = phenology.low.shallow)
#check assumptions
#autoplot(model.start.lowshallow)
#results on linear model
anova(model.start.lowshallow)
summary(model.start.lowshallow)




#start
phenology.gis.temperature <- phenology.gis.temperature[phenology.gis.temperature$LC.SLOPE == "high lc.steep slope", ]
table(phenology.gis.temperature$LC.SLOPE)
model_start<-lm(AV ~ daynum.tenth, data = phenology.gis.temperature)
#check assumptions
autoplot(model_start)
#results on linear model
anova(model_start)
summary(model_start)
