


#output of this script in outputs file C:/Users/dp005352/Documents/PhD/BMS_Data_Analysis/Outputs/steepchalk.pdf


###########################################################
Fitted_Values<-read.csv("Landscape_Phenology_Analysis/Data/GAM_Data/Meadow_Brown_Fitted_Values.txt",header=T)  
names(Fitted_Values)<-tolower(names(Fitted_Values))
head(Fitted_Values)
names(Fitted_Values)
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
nrow(gis.data)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
nrow(gis.data)
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
nrow(gis.data)
gis.data$LCper<-gis.data$LC/(pi*500^2)*100  


site.list.2003<-unique(Fitted_Values$transect_code[Fitted_Values$year==2003])
length(site.list.2003)

site.list.2011<-unique(Fitted_Values$transect_code[Fitted_Values$year==2011])
length(site.list.2011)

site.list.2003.2011<-site.list.2003[site.list.2003%in%site.list.2011]

length(site.list.2003.2011)  

#tail(with(Fitted_Values,table(transect_code,year)))
#head(with(Fitted_Values,table(transect_code,year)))

gis.data$LC.label[gis.data$LCper<(3.108)]<-"low lc"  
gis.data$LC.label[gis.data$LCper>(3.108)]<-"high lc"  

gis.data<-gis.data[(gis.data$SLOPE_MEAN >=0),]#remove values below 0
gis.data$SLOPE_MEAN.label[gis.data$SLOPE_MEAN<(5.03)]<-"shallow slope"  
gis.data$SLOPE_MEAN.label[gis.data$SLOPE_MEAN>(5.03)]<-"steep slope"  

for (i in 1:nrow(gis.data)){
  gis.data$LC.SLOPE[i]<-paste(gis.data[i,"LC.label"],gis.data[i,"SLOPE_MEAN.label"],sep=".")
}
nrow(gis.data[gis.data$LC.SLOPE=="high lc.steep slope",])
table(gis.data$LC.SLOPE)
gis.data<-gis.data[c(1,89)]
head(gis.data)
gis.data$siteno.gref <- as.numeric(as.character(gis.data$siteno.gref))
gis.data<-gis.data[order(gis.data$siteno.gref),]


site.list<-unique(gis.data[gis.data$LC.SLOPE=="high lc.steep slope",1])
site.list
length(site.list)
site.list<-site.list[site.list%in%site.list.2003.2011]
length(site.list)
site.list
site.list<-as.numeric((as.character(site.list)))
site.list<-site.list[order(site.list)]
#graphics.off()

#windows(8,14)

#devAskNewPage(ask=TRUE)
pdf("Landscape_Phenology_Analyis/Outputs/shallow_low_chalk.pdf")
par(mfrow=c(3,2))
for(s in site.list){
site.table<-Fitted_Values[Fitted_Values$transect_code==s,]
   for (y in c(2003,2011)){
    site.year.table<-site.table[site.table$year==y,]  
    
    site.list<-site.list[order(site.list)]
    
    plot(site.year.table$fit_count~site.year.table$daynum,type="l",main=paste(s,y),xlab="Day Number",ylab="Fitted Count",xlim=c(-50,200),ylim=c(0,800)) 

    site.year.table$fit_count.cum<-cumsum(site.year.table$fit_count) 
    site.year.table$standardised.fit_count.cum<-site.year.table$fit_count.cum/max(site.year.table$fit_count.cum)
    daynum.tenth<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.1))]
    daynum.median<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.5))]
    daynum.nintieth<-site.year.table$daynum[which.min(abs(site.year.table$standardised.fit_count.cum-0.9))]
    site.year.table$xw<-site.year.table$daynum*site.year.table$fit_count
    daynum.mean<-sum(site.year.table$xw)/sum(site.year.table$fit_count)
    daynum.range<-daynum.nintieth-daynum.tenth
    
    abline(v =daynum.tenth, col="green")     ; abline(v =daynum.nintieth, col="blue") ; abline(v =daynum.mean, col="black")   #;  abline(v =daynum.median, col="red") 
  
  }
}
dev.off()

site.year.table
