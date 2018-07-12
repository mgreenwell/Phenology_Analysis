library(tidyverse)
library(ggfortify)

#read in csv file of seasonal temperatures
temperature <- read.csv("Temperature_Phenology_Analysis/Data/seasonal.HADCET.mean.temps.csv", header = TRUE)

#check first six lines of datafream
head(temperature)

#create scatter graph of spring temperatures (MAM) vs summer temperatures (JJA) with line of best fit with 95% ci intervals
temperature_scatter <- ggplot(temperature, aes(x = MAM, y = JJA)) + 
  geom_point() + 
  geom_smooth(method = lm)

#plot scatter graph
temperature_scatter

#fit a lm to the data
model1 <- lm(MAM ~ JJA, data = temperature)

#test assumptions of linear model
autoplot(model1)

#check for overall significance of main interaction
anova(model1)
#check significance of coeficients
summary(model1)
