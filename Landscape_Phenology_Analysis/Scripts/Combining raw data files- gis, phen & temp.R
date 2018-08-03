
# script creates datasheet with temperature, gis and phenology all combined.
# found at write.csv(phenology.gis.temperature, file = "C:\\Users\\dp005352\\Dropbox\\PhD\\BMS_Data_Analysis\\Outputs\\combined_raw_data.csv", row.names = FALSE)

### load required packages

library(tidyverse)
library(ggfortify)
library()
library()

### read in dataframes

temperature <-
  read.csv(
    "C://Users//dp005352//Dropbox//PhD//BMS_Data_Analysis//Temperature_Data//seasonal.HADCET.mean.temps.csv",
    header = T
  )

gis.data <-
  read.table(
    "C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",
    header = T
  )

# filter gis.data
gis.data <- gis.data[gis.data$Surv == "UKBMS", ]    #only ukbms rows
gis.data <- gis.data[gis.data$buffer == "500", ]   #only 500 buffer rows
gis.data$LCper <- gis.data$LC / (pi * 500 ^ 2) * 100

phenology <-
  read.csv(
    "C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\meadow_brown_phenology.csv",
    header = T
  )  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)

# merge data sets
phenology.gis <-
  merge(phenology, gis.data, by.x = "site", by.y = "siteno.gref") #merge datasets

head(phenology.gis)
head(temperature)


phenology.gis.temperature <- 
  merge(phenology.gis, temperature, by.x = "year", by.y = "year")

head(phenology.gis.temperature)

phenology.gis.temperature$AV = apply(phenology.gis.temperature[,c(93:96)],1,mean)

write.csv(phenology.gis.temperature, file = "C:\\Users\\dp005352\\Dropbox\\PhD\\BMS_Data_Analysis\\Outputs\\combined_raw_data.csv", row.names = FALSE)


