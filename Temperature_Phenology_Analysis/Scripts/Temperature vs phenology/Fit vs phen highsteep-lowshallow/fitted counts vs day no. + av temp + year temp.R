temp.fitted<-read.csv("Temperature_Phenology_Analysis/Outputs/temp.fitted.csv",header=T)  
head(temp.fitted)
tail(temp.fitted)

par(mfrow = c(3, 2)) 
for (s in sort(unique(temp.fitted$transect_code))[1:9]){      #insert [1:9] between last two brackets { i.e. ))[1:9]) } to only print sites 1-9    #sorts all values by unique transect code
  site.table<-temp.fitted[temp.fitted$transect_code==s,]     #create table using unique transect codes
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

plot(site.year.table$fit_count~site.year.table$daynum,type="l",main=paste(s,y),xlab="Day Number",ylab = "Fitted Count") 
#abline(v =daynum.tenth, col="green")     ;  abline(v =daynum.median, col="red") ; abline(v =daynum.nintieth, col="blue") ; abline(v =daynum.mean, col="black")

par(new = T)
plot(site.year.table$total.average~site.year.table$daynum,type="l",axes=F,main=paste(s,y),xlab=NA,ylab=NA) 
abline(h=site.year.table$yearly.average, col="red")
axis(side = 4)
mtext( side = 4, line = 2,'Temperature')

 
}
}

