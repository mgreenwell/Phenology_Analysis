library(tidyverse)
library(ggfortify)
library(gridExtra)
library(lme4)

phenology.gis.temperature <-
  read.csv(
    "Landscape_Phenology_Analysis/Outputs/combined_raw_data.csv",header = T)
  

### define high and low chalk

summary(phenology.gis.temperature$LCper)
phenology.gis.temperature$LC.label[phenology.gis.temperature$LCper < (3.108)] <- "low lc"
phenology.gis.temperature$LC.label[phenology.gis.temperature$LCper > (3.108)] <- "high lc"
table(phenology.gis.temperature$LC.label)


###  define steep and shallow slope

summary(phenology.gis.temperature$SLOPE_MEAN)
phenology.gis.temperature$SLOPE_MEAN.label[phenology.gis.temperature$SLOPE_MEAN < (5.03)] <- "shallow slope"

phenology.gis.temperature$SLOPE_MEAN.label[phenology.gis.temperature$SLOPE_MEAN > (5.03)] <- "steep slope"

table(phenology.gis.temperature$SLOPE_MEAN.label)


### define north and south facing


phenology.gis.temperature$NORTHNESS_MEAN.label[phenology.gis.temperature$NORTHNESS_MEAN < (-0.5)] <- "south_facing"
phenology.gis.temperature$NORTHNESS_MEAN.label[phenology.gis.temperature$NORTHNESS_MEAN > (0.5)] <- "north_facing"

table(phenology.gis.temperature$NORTHNESS_MEAN.label)


phenology.gis.temperature$NORTHNESS_MEAN.label[is.na(phenology.gis.temperature$NORTHNESS_MEAN.label)] <- "other"
table(phenology.gis.temperature$NORTHNESS_MEAN.label)

### define steep chalk  etc.

for (i in 1:nrow(phenology.gis.temperature)) {
  phenology.gis.temperature$LC.ASPECT[i] <-
    paste(phenology.gis.temperature[i, "LC.label"],
          phenology.gis.temperature[i, "NORTHNESS_MEAN.label"],
          sep = ".")
}
table(phenology.gis.temperature$LC.ASPECT)
# head(phenology.gis.temperature)


## remove other
phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT == "low lc.other",]

phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT == "high lc.other",]

table(phenology.gis.temperature$LC.ASPECT)

## remove factors not interested in

phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT == "low lc.south_facing",]

phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT == "high lc.north_facing",]

table(phenology.gis.temperature$LC.ASPECT)



# bind labels 

for (i in 1:nrow(phenology.gis.temperature)) {
  phenology.gis.temperature$LC.ASPECT.SLOPE[i] <-
    paste(phenology.gis.temperature[i, "LC.ASPECT"],
          phenology.gis.temperature[i, "SLOPE_MEAN.label"],
          sep = ".")
}

table(phenology.gis.temperature$LC.ASPECT.SLOPE)

#remove unwanted variables
phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT.SLOPE == "high lc.south_facing.shallow slope",]

phenology.gis.temperature <-
  phenology.gis.temperature[!phenology.gis.temperature$LC.ASPECT.SLOPE == "low lc.north_facing.steep slope",]

table(phenology.gis.temperature$LC.ASPECT.SLOPE)

####plotting graphs

start_date <-
  ggplot(phenology.gis.temperature,
         aes(x = JJA, y = daynum.tenth, colour = LC.ASPECT.SLOPE)) +
  geom_point(shape = 16) +
  geom_smooth(method = lm) + 
  ylab("Day") + 
  xlab("Temperature ?C") + 
  ggtitle("Summer average temperature vs start date") +
  theme(legend.title = element_blank())
start_date

end_date <-
  ggplot(phenology.gis.temperature,
         aes(x = JJA, y = daynum.nintieth, colour = LC.ASPECT.SLOPE)) +
  geom_point(shape = 16) +
  geom_smooth(method = lm) + 
  ylab("Day") + 
  xlab("Temperature ?C") + 
  ggtitle("Summer average temperature vs end date") +
  theme(legend.title = element_blank())
end_date

mean_date <-
  ggplot(phenology.gis.temperature,
         aes(x = JJA, y = daynum.mean, colour = LC.ASPECT.SLOPE)) +
  geom_point(shape = 16) +
  geom_smooth(method = lm) + 
  ylab("Day") + 
  xlab("Temperature ?C") + 
  ggtitle("Summer average temperature vs mean date") +
  theme(legend.title = element_blank())
mean_date

range_date <-
  ggplot(phenology.gis.temperature,
         aes(x = JJA, y = daynum.range, colour = LC.ASPECT.SLOPE)) +
  geom_point(shape = 16) +
  geom_smooth(method = lm) + 
  ylab("Days") + 
  xlab("Temperature ?C") + 
  ggtitle("Summer average temperature vs flight period length") +
  theme(legend.title = element_blank())
range_date

grid.arrange(start_date, end_date, mean_date, range_date)


#### modeling

#run linear model
spring.summer.end <- lm(daynum.nintieth ~ MAM + JJA, data = phenology.gis.temperature)
anova(spring.summer.end)
summary(spring.summer.end)

#test model assumptions
autoplot(spring.summer.end)


#run mixed effects model
model<-lmer(daynum.nintieth ~ MAM + JJA * LC.ASPECT.SLOPE + (1|site), phenology.gis.temperature)
summary(model)

#simplify model, remove slope type
model2<-lmer(daynum.nintieth ~ MAM + JJA + (1|site), phenology.gis.temperature)
summary(model2)

#comapre models
anova(model, model2)
