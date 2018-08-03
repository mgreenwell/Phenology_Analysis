# script creates
#results<-read.csv("C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\meadow_brown_phenology.csv",header=T)
#head(results)

#################


Fitted_Values<-read.csv("C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\GAM_Data\\Meadow_Brown_Fitted_Values.txt",header=T)  
names(Fitted_Values)<-tolower(names(Fitted_Values))
head(Fitted_Values)

results<-NULL
par(mfrow = c(3, 3)) 
for (s in sort(unique(Fitted_Values$transect_code))[1:9]){      #insert [1:9] between last two brackets { i.e. ))[1:9]) } to only print sites 1-9    #sorts all values by unique transect code
  site.table<-Fitted_Values[Fitted_Values$transect_code==s,]     #create table using unique transect codes
  for (y in sort(unique(site.table$year))){                      #sorts table by year for each site
    site.year.table<-site.table[site.table$year==y,]  
    
    site.year.table$fit_count.cum<-cumsum(site.year.table$fit_count) 
    site.year.table$standardised.fit_count.cum<-site.year.table$fit_count.cum/max(site.year.table$fit_count.cum)
    daynum.tenth<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.1))]
    daynum.median<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.5))]
    daynum.nintieth<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.9))]
    
    site.year.table$xw<-site.year.table$daynum*site.year.table$fit_count
    daynum.mean<-sum(site.year.table$xw)/sum(site.year.table$fit_count)
    
    daynum.range<-daynum.nintieth-daynum.tenth
    plot(site.year.table$standardised.fit_count.cum~site.year.table$daynum,type="l",main=paste(s,y),xlab="Day Number",ylab="Standardised Cumulative Fitted Count")
    abline(v =daynum.tenth, col="green")     ;  abline(v =daynum.median, col="red") ; abline(v =daynum.nintieth, col="blue") ; abline(v =daynum.mean, col="black")  
    
    plot(site.year.table$fit_count~site.year.table$daynum,type="l",main=paste(s,y),xlab="Day Number",ylab="Fitted Count") 
    abline(v =daynum.tenth, col="green")     ;  abline(v =daynum.median, col="red") ; abline(v =daynum.nintieth, col="blue") ; abline(v =daynum.mean, col="black")
site<-s
year<-y
    
     results.temp<-data.frame(site,year,daynum.tenth,daynum.median,daynum.nintieth,daynum.mean,daynum.range)
     results<-rbind(results,results.temp)
    
    }
}
head(results)
#write.csv(results,"C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\Outputs\\meadow_brown_phenology.csv",row.names=F)


