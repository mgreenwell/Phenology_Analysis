library(tidyverse)
library(gridExtra)

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


############################ allocating values ################################
## Chalk

summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(4.1273)]<-"Non Chalk"  
phenology.gis$LC.label[phenology.gis$LCper>(4.1273)]<-"Chalk"  

## Slope

#remove values below 0
phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]
summary(phenology.gis$SLOPE_MEAN)
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  

############################ assigning labels #################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.SLOPE[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}
table(phenology.gis$LC.SLOPE)

##########################  plotting graphs  ##################################

## Start date

start.date <- ggplot(phenology.gis,
                  aes(x = LC.SLOPE, y = daynum.tenth)) +
  geom_boxplot() +
  ylab("Flight Period Start Day") +
  theme_classic() +
  xlab("Site Conditions") + 
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

start.date

## End Date

end.date <- ggplot(phenology.gis,
                  aes(x = LC.SLOPE, y = daynum.tenth)) +
  geom_boxplot() +
  ylab("Flight Period End Day") +
  theme_classic() +
  xlab("Site Conditions") + 
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

end.date

## Mean Date

mean.date <- ggplot(phenology.gis,
                  aes(x = LC.SLOPE, y = daynum.mean)) +
  geom_boxplot() +
  ylab("Flight Period Mean Day") +
  theme_classic() +
  xlab("Site Conditions") + 
  annotate("text", x = "Chalk.shallow slope", y = 165, label = "A") +
  annotate("text", x = "Chalk.steep slope", y = 165, label = "B") +
  annotate("text", x = "Non Chalk.shallow slope", y = 165, label = "C") +
  annotate("text", x = "Non Chalk.steep slope", y = 165, label = "A") +
  scale_x_discrete(
    labels = c(
      "Chalk.shallow slope" = "Shallow Slope\nChalk",
      "Chalk.steep slope" = "Steep Slope\nChalk",
      "Non Chalk.shallow slope" = "Shallow Slope\nNon Chalk",
      "Non Chalk.steep slope" = "Steep Slope\nNon Chalk"
    ))

mean.date

## Range


range.date <- ggplot(phenology.gis,
                  aes(x = LC.SLOPE, y = daynum.range)) +
  geom_boxplot() +
  ylab("Flight Period Range") +
  theme_classic() +
  xlab("Site conditions") + 
  annotate("text", x = "Chalk.shallow slope", y = 125, label = "A") +
  annotate("text", x = "Chalk.steep slope", y = 125, label = "B") +
  annotate("text", x = "Non Chalk.shallow slope", y = 125, label = "C") +
  annotate("text", x = "Non Chalk.steep slope", y = 125, label = "A") +
  scale_x_discrete(
    labels = c(
      "Chalk.shallow slope" = "Shallow Slope\nChalk",
      "Chalk.steep slope" = "Steep Slope\nChalk",
      "Non Chalk.shallow slope" = "Shallow Slope\nNon Chalk",
      "Non Chalk.steep slope" = "Steep Slope\nNon Chalk"
    ))

range.date


grid.arrange(
  start.date,
  mean.date,
  end.date,
  range.date,
  ncol = 2,
  nrow = 2
)

