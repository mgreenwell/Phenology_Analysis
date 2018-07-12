# Script creates output file
#   <-read.csv("C:\\Users\\matgre\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\temperature.data.csv",header=T)


###can't find file yearly_temperature_averages

temperature.av<-read.csv("C:\\Users\\matgre\\Documents\\PhD\\BMS_Data_Analysis\Temperature_Data\\\yearly_temperature_averages.csv",header=T)
names(temperature.av)<-tolower(names(temperature.av))
head(temperature.av)

temperature<-read.csv("C:\\Users\\matgre\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\year_temp.csv",header=T)
names(temperature)<-tolower(names(temperature))
head(temperature)

temperature.data<-merge(temperature,temperature.av, by.x="year",by.y="year")

head(temperature.data)
temperature.data


temperature.data$total.average<-mean(temperature.data$temperature)

temperature.data

#write.csv(temperature.data,"C:\\Users\\matgre\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\temperature.data.csv",row.names=F)
