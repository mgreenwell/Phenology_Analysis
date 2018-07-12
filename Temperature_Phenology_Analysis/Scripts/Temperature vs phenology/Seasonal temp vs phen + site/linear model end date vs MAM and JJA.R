#load packages
library(tidyverse)
library(ggfortify)
library(gridExtra)
library(lme4)

all.sites <- 
  read.csv(
    "Landscape_Phenology_Analysis/Outputs/combined_raw_data.csv",header = T)



head(all.sites)

#define chalk types
summary(all.sites$LCper)
all.sites$LC.label[all.sites$LCper<(3.108)]<-"low lc"  
all.sites$LC.label[all.sites$LCper>(3.108)]<-"high lc"  
table(all.sites$LC.label)


###  define steep and shallow slope

summary(all.sites$SLOPE_MEAN)
all.sites$SLOPE_MEAN.label[all.sites$SLOPE_MEAN<(5.03)]<-"shallow slope"  
all.sites$SLOPE_MEAN.label[all.sites$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(all.sites$SLOPE_MEAN.label)


### define steep chalk  etc.

for (i in 1:nrow(all.sites)) {
  all.sites$LC.SLOPE[i] <-
    paste(all.sites[i, "LC.label"],
          all.sites[i, "SLOPE_MEAN.label"],
          sep = ".")
}
table(all.sites$LC.SLOPE)
# head(all.sites)


## remove high chalk, low altitude and low chalk, high altitude

all.sites <- all.sites[!all.sites$LC.SLOPE == "low lc.steep slope", ]
all.sites <- all.sites[!all.sites$LC.SLOPE == "high lc.shallow slope", ]
table(all.sites$LC.SLOPE)

##tidy data table
all.sites <- select(all.sites, year, site, daynum.tenth, daynum.mean, daynum.nintieth, daynum.range, DJF, MAM, JJA, SON, AV, LC.SLOPE)
head(all.sites)
#create
model1 <- lmer(daynum.nintieth ~ AV * LC.SLOPE + (1| site), all.sites )
summary(model1)

lmer()
