# script writes three dataframes in below locations. phenology.temperature is most useful, other two purely for graphing in "testing correlations between seasonal temperatures and phenology graphs" script
# 
# write.csv(phenology.temperature, file = "C:\\Users\\dp005352\\Dropbox\\PhD\\BMS_Data_Analysis\\Outputs\\Temperature_vs_Phenology\\merged_data_set_of_phenology_and_temperature.csv", row.names = FALSE)
# write.csv(phenology.temperature.fourway, file = "C:\\Users\\dp005352\\Dropbox\\PhD\\BMS_Data_Analysis\\Outputs\\Temperature_vs_Phenology\\merged_data_set_of_phenology_and_temperature_for_fourway_plot.csv", row.names = FALSE)
# write.csv(phenology.temperature.individuals, file = "C:\\Users\\dp005352\\Dropbox\\PhD\\BMS_Data_Analysis\\Outputs\\Temperature_vs_Phenology\\merged_data_set_of_phenology_and_temperature_for_individual_plots.csv", row.names = FALSE)




#load required packages
library(ggplot2)
library(plyr)
library(reshape)
library(ggfortify)

#loadf in raw data
phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)
temperature<-read.csv("Temperature_Phenology_Analysis/Data/seasonal.HADCET.mean.temps.csv",header=T)

#head(phenology)
#head(temperature)
########################sorting data frames into working formats##########
#forming three dataframes
#phenology.temperature                  dataframe for quadplot
#phenology.temperature.fourway          dataframe for individual graphs in quadplot
#phenology.temperature.individuals      dataframe for individual plots and lms



# take the average daynum.tenth across all sites and create dataframe of mean across all sites for each year.
#repeated for all phenology types.

tenth.means<-aggregate(phenology[, 3], list(phenology$year), mean)
tenth.means<-rename(tenth.means,c("Group.1"="year","x"="average.tenth"))
#tenth.means

nintieth.means<-aggregate(phenology[, 5], list(phenology$year), mean)
nintieth.means<-rename(nintieth.means,c("Group.1"="year","x"="average.nintieth"))
#nintieth.means

mean.means<-aggregate(phenology[, 6], list(phenology$year), mean)
mean.means<-rename(mean.means,c("Group.1"="year","x"="average.mean"))
#mean.means

range.means<-aggregate(phenology[, 7], list(phenology$year), mean)
range.means<-rename(range.means,c("Group.1"="year","x"="average.range"))
#range.means



#merge the four new dataframes to make one dataframe with all averages.
tenth.mean<-merge(tenth.means,mean.means, by.x="year")
nintieth.range<-merge(nintieth.means, range.means, by.x="year")
phenology.means<-merge(tenth.mean,nintieth.range, by.x="year")
head(phenology.means)

#rearrange data frame
phenology.means.melt <- melt(phenology.means, id=(c("year")))
phenology.means.melt<-rename(phenology.means.melt,c("variable"="flight.period","value"="length.date"))
head(phenology.means.melt)
#phenology.means

#temperature
# remove all values in df before 1976
temperature<-temperature[!temperature$year < "1976", ]
head(temperature)
#reorder dataset so that three columns year (year) variable (JJA etc) and value (temperature value)
temperature.melt<-melt(temperature, id=(c("year")))
head(temperature.melt)

#rename variable to make season and value to make temperature
temperature.melt<-rename(temperature.melt,c("variable"="season","value"="temperature"))

#rename seasons to show spring etc.
temperature.melt$season<- revalue(temperature.melt$season, c("MAM"="Spring (MAM)","JJA"="Summer (JJA)","SON"="Autumn (SON)","DJF"="Winter (DJF)"))
head(temperature.melt)

#merge phenology.means.melt and temperature
phenology.temperature<-merge(phenology.means.melt,temperature.melt, by.x="year")
head(phenology.temperature)

#rename flight.period values to make more sense
phenology.temperature$flight.period<- revalue(phenology.temperature$flight.period, c("average.tenth"="Start day","average.nintieth"="End day","average.mean"="Mean day","average.range"="Flight period length"))
head(phenology.temperature)


#create new df. expand phenology.temperature
phenology.temperature.fourway<-merge(phenology.means.melt,temperature,by.x="year")
head(phenology.temperature.fourway)

#create new df. expand phenology.temperature.fourway
phenology.temperature.individuals<-merge(phenology.means,temperature, by.x="year")
head(phenology.temperature.individuals)

#write.csv(phenology.temperature, file = "Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature.csv", row.names = FALSE)
#write.csv(phenology.temperature.fourway, file = "Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature_for_fourway_plot.csv", row.names = FALSE)
#write.csv(phenology.temperature.individuals, file = "Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature_for_individual_plots.csv", row.names = FALSE)




