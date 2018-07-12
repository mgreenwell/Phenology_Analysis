temperature<-read.csv("Temperature_Phenology_Analysis/Outputs/temperature.data.csv",header=T)
names(temperature)<-tolower(names(temperature))
head(temperature)
#temperature$month  = factor(temperature$month, levels=c("jan", "feb", "mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))


par(mfrow = c(3, 3)) 
for (y in sort(unique(temperature$year))[1:9]){ 
  temp.table<-temperature[temperature$year==y,]
plot(temp.table$monthly.temperature~temp.table$month,main=paste(y),xlab = "month") 
  }




#for (y in sort(unique(temperature$year))){ 
 # temp.table<-temperature[temperature$year==y,]
  #plot(temp.table$monthly.temperature,main=paste(y),xlab = "month") 

#}

