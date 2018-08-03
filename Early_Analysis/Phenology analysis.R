search()
ls()
gis.data<-read.table("C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC


phenology<-read.csv("C:\\Users\\dp005352\\Documents\\PhD\\BMS_Data_Analysis\\Early_Analysis\\pheno_var_2012_good_sp.txt",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)
names(phenology)<-tolower(names(phenology))
phenology<-phenology[phenology$common_name=="Meadow Brown",]     #only meadow brown rows
phenology$north<-phenology$north/1000                            #north value into km
phenology.gis<-merge(phenology,gis.data,by.x="siteno",by.y="siteno.gref") #merge datasets

#work out most northern value with LC%>0
#test<-phenology.gis[,c(4,94)]
#head(test)
#test[test ==0] <- NA
#head(test)
#na.omit(test)
#head(test)
#test<-na.omit(test)
#head(test)
#summary(test$north)
# most norhtern LC%>0 =626.0

#how many rows before removal of northen sites
nrow(phenology.gis)
phenology.gis<-phenology.gis[(phenology.gis$north <=626),]
summary(phenology.gis$north)
#rows after northern sites removed
nrow(phenology.gis)

summary(phenology.gis$LCper)
#hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,8000), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")

phenology.gis$LC.label[phenology.gis$LCper<(5)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(5)]<-"high lc"  
table(phenology.gis$LC.label)

summary(phenology.gis$DEM_MEAN)
nrow(phenology.gis)
phenology.gis<-phenology.gis[(phenology.gis$DEM_MEAN >=0),] #remove values below 0
nrow(phenology.gis)
summary(phenology.gis$DEM_MEAN)
#hist(phenology.gis$DEM_MEAN,breaks=40,col="red",ylim =c(0,2000), xlab = "mean hight above sealevel (m)",main="Histogram of hight above sealevel")

phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN<(150)]<-"low altitude"  
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN>(150)]<-"high altitude"  
table(phenology.gis$DEM_MEAN.label)

summary(phenology.gis$SLOPE_MEAN)
nrow(phenology.gis)
phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]#remove values below 0
nrow(phenology.gis)
summary(phenology.gis$DEM_MEAN)
#hist(phenology.gis$SLOPE_MEAN,breaks=40,col="red",ylim =c(0,2000), xlab = "Mean slope, degrees from horizontal",main="Histogram of mean slope angle")

phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(10)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(10)]<-"steep slope"  
table(phenology.gis$SLOPE_MEAN.label)


###does meanday need editing to remove values below zero?###


#summary(phenology.gis$meanday)
#phenology.gis<-phenology.gis[(phenology.gis$meanday >0),]
#summary(phenology.gis$meanday)



#####################################################################
model<-lm(meanday~LCper+factor(year),phenology.gis)
with(phenology.gis,plot(meanday~LCper, xlab="Calcarous grassland area %",main="calcareous grassland area % vs meanday"))

model<-lm(meanday~DEM_MEAN+factor(year),phenology.gis)
with(phenology.gis,plot(meanday~DEM_MEAN, xlab="Mean site altitude",main="Altitude vs meanday"))

model<-lm(meanday~NORTHNESS_MEAN+factor(year),phenology.gis)
with(phenology.gis,plot(meanday~NORTHNESS_MEAN, xlab="Northness (1=due north)",main="Northness vs meanday"))

model<-lm(meanday~north+factor(year),phenology.gis)
with(phenology.gis,plot(meanday~north, xlab="Northing (km)",main="Northing vs meanday"))

model<-lm(meanday~SLOPE_MEAN+factor(year),phenology.gis)
with(phenology.gis,plot(meanday~SLOPE_MEAN, xlab="Mean slope angle (degrees)",main="mean slope angel vs meanday"))

par(mfrow=c(2,2))
with(phenology.gis,plot(meanday~LCper, xlab="Calcarous grassland area %",main="calcareous grassland area % vs meanday"))
with(phenology.gis,plot(meanday~DEM_MEAN, xlab="Mean site altitude",main="Altitude vs meanday"))
with(phenology.gis,plot(meanday~NORTHNESS_MEAN, xlab="Northness (1=due north)",main="Northness vs meanday"))
with(phenology.gis,plot(meanday~north, xlab="Northing (km)",main="Northing vs meanday"))
with(phenology.gis,plot(meanday~SLOPE_MEAN, xlab="Mean slope angle (degrees)",main="mean slope angel vs meanday"))


#head(phenology.gis)

#phenology.gis$LC.label<-"test"
#phenology.gis$spp.3<-NA
for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.SLOPE[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}
table(phenology.gis$LC.SLOPE)
names(phenology.gis)

library(lme4)
model<-lmer(meanday~LC.SLOPE+year+(1|siteno),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
with(phenology.gis, boxplot(meanday~LC.SLOPE))
#?barplot
#with(phenology.gis,barplot(meanday~LC.SLOPE))


### reached this point###


modelnull<-lmer(meanday~year+(1|siteno),phenology.gis)
anova(model,modelnull)
#Testing whether LC.SLOPE has an effect on meanday by comparing with a model that just looks at meanday vs year with site as random effect
#result shows sig dif between models.
#AIC lower on original model therefore more of results explained by model.


phenology.gis$LC.SLOPE2<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE2<-as.factor(phenology.gis$LC.SLOPE2)
levels(phenology.gis$LC.SLOPE2)

#group together
levels(phenology.gis$LC.SLOPE2)<-c("A","B","C","A")
levels(phenology.gis$LC.SLOPE2)

model2<-lmer(meanday~LC.SLOPE2+year+(1|siteno),phenology.gis)
anova(model,model2)
#results show no sig diff between av meanday of highlc.shallow and lowlc.steep

phenology.gis$LC.SLOPE3<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE3<-as.factor(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)<-c("A","B","A","A")
levels(phenology.gis$LC.SLOPE3)
model3<-lmer(meanday~LC.SLOPE3+year+(1|siteno),phenology.gis)
anova(model2,model3)
#results show sig diff between models with model2 being better fit

phenology.gis$LC.SLOPE4<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE4<-as.factor(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)<-c("A","A","C","A")
levels(phenology.gis$LC.SLOPE4)
model4<-lmer(meanday~LC.SLOPE4+year+(1|siteno),phenology.gis)
anova(model2,model4)
#results show sig diff between models with model2 being better fit

phenology.gis$LC.SLOPE5<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE5<-as.factor(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)<-c("A","B","B","A")
levels(phenology.gis$LC.SLOPE5)
model5<-lmer(meanday~LC.SLOPE5+year+(1|siteno),phenology.gis)
anova(model2,model5)
###   have shown that simplest model is model 2.



with(phenology.gis, boxplot(meanday~LC.SLOPE2,xlab="Site type",ylab="meanday",ylim = c(-10, 200)))
#plot boxplot of model2

barchart<-with(phenology.gis,tapply(meanday,LC.SLOPE2,mean))
#barplot(barchart,xlab = "LC.SLOPE",ylab = "meanday",ylim=c(0,150))
#plots basic barplot without error bars


#myData <- aggregate(phenology.gis$meanday,
# by = list(LC.SLOPE2 = phenology.gis$LC.SLOPE2),
#FUN = function(meanday) c(mean = mean(meanday), sd = sd(meanday),
#                    n = length(meanday)))
###    calculates mean, SD and n for each category (ABC)    ###
#head(myData)

#myData <- do.call(data.frame, myData)
####converts into matrix
#head(myData)

#myData$se <- myData$x.sd / sqrt(myData$x.n)
#head(myData)
###   calcualtes standard error and adds column
#(myData) <- c("LC.SLOPE","mean", "sd", "n", "se")
###    renames columns
#head(myData)

library(plyr)
bargraph<-ddply(phenology.gis, c("LC.SLOPE2"),summarise,
                N=length(meanday),
                mean=mean(meanday),
                sd=sd(meanday),
                se=sd/ sqrt(N))
head(bargraph)
?ddply
#shorter code works out the same (N,sd,se,mean)

library(ggplot2)

ggplot(bargraph, aes(x = LC.SLOPE2, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=0.1)) +
  ggtitle("Meanday vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="Meanday") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

p<-ggplot(phenology.gis,aes(x=LC.SLOPE2,y=meanday))+geom_violin(fill="grey",col="black")+theme_minimal()
p + geom_boxplot(width=0.1) + expand_limits(y=c(-10,200)) + scale_y_continuous(breaks=seq(0, 200, 20))+ggtitle("Meanday vs sight Type")+xlab("Site Category")+ylab("meanday")

test<-ddply(phenology.gis, c("LC.SLOPE2"),summarise,
            N=length(meanday),
            mean=mean(meanday),
            sd=sd(meanday),
            se=sd/ sqrt(N),
            min = min(meanday),
            max=max(meanday))
head(test)

#midxed model and anova chapters
##p values for mixed models
##levels and grouping factor levels