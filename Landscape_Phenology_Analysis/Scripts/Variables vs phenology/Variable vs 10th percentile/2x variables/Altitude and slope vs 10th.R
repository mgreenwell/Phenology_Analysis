search()
ls()
gis.data<-read.table("Landscape_Phenology_Analysis/data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
gis.data<-gis.data[gis.data$Surv=="UKBMS",]    #only ukbms rows
gis.data<-gis.data[gis.data$buffer=="500",]   #only 500 buffer rows
gis.data$LCper<-gis.data$LC/(pi*500^2)*100            #Add row - convert LC to percentage of LC
#head(gis.data)
#summary(gis.data$LC)
#summary(gis.data$LCper)

phenology<-read.csv("Landscape_Phenology_Analysis/Outputs/meadow_brown_phenology.csv",header=T)  # limits to univoltine species, records where flight period is greater than zero i.e. the species was recorded more than once in the year at that site, and species occupy 10 or more  10km squares within the UKBMS (see email from Marc Botham  Wed 23/10/2013 14:33)


phenology.gis<-merge(phenology,gis.data,by.x="site",by.y="siteno.gref") #merge datasets

nrow(phenology.gis)


############################ allocating values ####################################


par(mfrow = c(1, 1))

#hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,6000),xlim =c(0,100), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")
summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(4.13)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(4.13)]<-"high lc"  
table(phenology.gis$LC.label)

#hist(phenology.gis$DEM_MEAN,breaks=40,col="red",ylim =c(0,1500), xlab = "mean hight above sealevel (m)",main="Histogram of hight above sealevel")
summary(phenology.gis$DEM_MEAN)
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN<(85.76)]<-"low altitude"  #using mean
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN>(85.76)]<-"high altitude"  
table(phenology.gis$DEM_MEAN.label)

summary(phenology.gis$SLOPE_MEAN)
nrow(phenology.gis)
phenology.gis<-phenology.gis[(phenology.gis$SLOPE_MEAN >=0),]#remove values below 0
nrow(phenology.gis)
#hist(phenology.gis$SLOPE_MEAN,breaks=40,col="red",ylim =c(0,700),xlim =c(0,25), xlab = "Mean slope, degrees from horizontal",main="Histogram of mean slope angle")
summary(phenology.gis$SLOPE_MEAN)
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN<(5.03)]<-"shallow slope"  
phenology.gis$SLOPE_MEAN.label[phenology.gis$SLOPE_MEAN>(5.03)]<-"steep slope"  
table(phenology.gis$SLOPE_MEAN.label)

head(phenology.gis)


############################ assigning labels sea level ######################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$DEM_MEAN.SLOPE[i]<-paste(phenology.gis[i,"DEM_MEAN.label"],phenology.gis[i,"SLOPE_MEAN.label"],sep=".")
}
table(phenology.gis$DEM_MEAN.SLOPE)
names(phenology.gis)

library(lme4)
model<-lmer(daynum.tenth~DEM_MEAN.SLOPE+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.tenth~DEM_MEAN.SLOPE))


############################## Testing models #############################


modelnull<-lmer(daynum.tenth~year+(1|site),phenology.gis)
anova(model,modelnull)
#Testing whether DEM_MEAN.SLOPE has an effect on daynum.tenth by comparing with a model that just looks at daynum.tenth vs year with site as random effect
#result shows sig dif between models.
#AIC lower on original model therefore more of results explained by model.

library(plyr)
bargraph<-ddply(phenology.gis, c("DEM_MEAN.SLOPE"),summarise,
                N=length(daynum.tenth),
                mean=mean(daynum.tenth),
                sd=sd(daynum.tenth),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)

ggplot(bargraph, aes(x = DEM_MEAN.SLOPE, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=4)) +
  ggtitle("daynum.tenth vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.tenth") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))


phenology.gis$DEM_MEAN.SLOPE2<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE2<-as.factor(phenology.gis$DEM_MEAN.SLOPE2)
levels(phenology.gis$DEM_MEAN.SLOPE2)
levels(phenology.gis$DEM_MEAN.SLOPE2)<-c("A","B","C","A")
levels(phenology.gis$DEM_MEAN.SLOPE2)
model2<-lmer(daynum.tenth~DEM_MEAN.SLOPE2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$DEM_MEAN.SLOPE3<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE3<-as.factor(phenology.gis$DEM_MEAN.SLOPE3)
levels(phenology.gis$DEM_MEAN.SLOPE3)
levels(phenology.gis$DEM_MEAN.SLOPE3)<-c("A","B","A","C")
levels(phenology.gis$DEM_MEAN.SLOPE3)
model3<-lmer(daynum.tenth~DEM_MEAN.SLOPE3+year+(1|site),phenology.gis)
anova(model,model3)

#######

phenology.gis$DEM_MEAN.SLOPE4<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE4<-as.factor(phenology.gis$DEM_MEAN.SLOPE4)
levels(phenology.gis$DEM_MEAN.SLOPE4)
levels(phenology.gis$DEM_MEAN.SLOPE4)<-c("A","A","B","C")
levels(phenology.gis$DEM_MEAN.SLOPE4)
model4<-lmer(daynum.tenth~DEM_MEAN.SLOPE4+year+(1|site),phenology.gis)
anova(model3,model4)

#######

phenology.gis$DEM_MEAN.SLOPE5<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE5<-as.factor(phenology.gis$DEM_MEAN.SLOPE5)
levels(phenology.gis$DEM_MEAN.SLOPE5)
levels(phenology.gis$DEM_MEAN.SLOPE5)<-c("B","C","A","A")
levels(phenology.gis$DEM_MEAN.SLOPE5)
model5<-lmer(daynum.tenth~DEM_MEAN.SLOPE5+year+(1|site),phenology.gis)
anova(model3,model5)

#######

phenology.gis$DEM_MEAN.SLOPE6<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE6<-as.factor(phenology.gis$DEM_MEAN.SLOPE6)
levels(phenology.gis$DEM_MEAN.SLOPE6)
levels(phenology.gis$DEM_MEAN.SLOPE6)<-c("B","A","C","A")
levels(phenology.gis$DEM_MEAN.SLOPE6)
model6<-lmer(daynum.tenth~DEM_MEAN.SLOPE6+year+(1|site),phenology.gis)
anova(model3,model6)

#######

phenology.gis$DEM_MEAN.SLOPE7<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE7<-as.factor(phenology.gis$DEM_MEAN.SLOPE7)
levels(phenology.gis$DEM_MEAN.SLOPE7)
levels(phenology.gis$DEM_MEAN.SLOPE7)<-c("B","A","A","C")
levels(phenology.gis$DEM_MEAN.SLOPE7)
model7<-lmer(daynum.tenth~DEM_MEAN.SLOPE7+year+(1|site),phenology.gis)
anova(model3,model7)

#######

phenology.gis$DEM_MEAN.SLOPE8<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE8<-as.factor(phenology.gis$DEM_MEAN.SLOPE8)
levels(phenology.gis$DEM_MEAN.SLOPE8)
levels(phenology.gis$DEM_MEAN.SLOPE8)<-c("A","A","B","B")
levels(phenology.gis$DEM_MEAN.SLOPE8)
model8<-lmer(daynum.tenth~DEM_MEAN.SLOPE8+year+(1|site),phenology.gis)
anova(model3,model8)

#######

phenology.gis$DEM_MEAN.SLOPE9<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE9<-as.factor(phenology.gis$DEM_MEAN.SLOPE9)
levels(phenology.gis$DEM_MEAN.SLOPE9)
levels(phenology.gis$DEM_MEAN.SLOPE9)<-c("A","B","A","B")    #("High alt.shallow & low alt.shallow","High alt.steep & Low alt.steep","High alt.shallow & low alt.shallow","High alt.steep & Low alt.steep")
levels(phenology.gis$DEM_MEAN.SLOPE9)
model9<-lmer(daynum.tenth~DEM_MEAN.SLOPE9+year+(1|site),phenology.gis)
anova(model3,model9)

#######

phenology.gis$DEM_MEAN.SLOPE10<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE10<-as.factor(phenology.gis$DEM_MEAN.SLOPE10)
levels(phenology.gis$DEM_MEAN.SLOPE10)
levels(phenology.gis$DEM_MEAN.SLOPE10)<-c("A","B","B","A")
levels(phenology.gis$DEM_MEAN.SLOPE10)
model10<-lmer(daynum.tenth~DEM_MEAN.SLOPE10+year+(1|site),phenology.gis)
anova(model9,model10)


#######

phenology.gis$DEM_MEAN.SLOPE11<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE11<-as.factor(phenology.gis$DEM_MEAN.SLOPE11)
levels(phenology.gis$DEM_MEAN.SLOPE11)
levels(phenology.gis$DEM_MEAN.SLOPE11)<-c("A","A","A","B")
levels(phenology.gis$DEM_MEAN.SLOPE11)
model11<-lmer(daynum.tenth~DEM_MEAN.SLOPE11+year+(1|site),phenology.gis)
anova(model9,model11)

#######

phenology.gis$DEM_MEAN.SLOPE12<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE12<-as.factor(phenology.gis$DEM_MEAN.SLOPE12)
levels(phenology.gis$DEM_MEAN.SLOPE12)
levels(phenology.gis$DEM_MEAN.SLOPE12)<-c("A","A","B","A")
levels(phenology.gis$DEM_MEAN.SLOPE12)
model12<-lmer(daynum.tenth~DEM_MEAN.SLOPE12+year+(1|site),phenology.gis)
anova(model9,model12)

#######

phenology.gis$DEM_MEAN.SLOPE13<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE13<-as.factor(phenology.gis$DEM_MEAN.SLOPE13)
levels(phenology.gis$DEM_MEAN.SLOPE13)
levels(phenology.gis$DEM_MEAN.SLOPE13)<-c("A","B","A","A")
levels(phenology.gis$DEM_MEAN.SLOPE13)
model13<-lmer(daynum.tenth~DEM_MEAN.SLOPE13+year+(1|site),phenology.gis)
anova(model9,model13)

#######

phenology.gis$DEM_MEAN.SLOPE14<-phenology.gis$DEM_MEAN.SLOPE
phenology.gis$DEM_MEAN.SLOPE14<-as.factor(phenology.gis$DEM_MEAN.SLOPE14)
levels(phenology.gis$DEM_MEAN.SLOPE14)
levels(phenology.gis$DEM_MEAN.SLOPE14)<-c("B","A","A","A")
levels(phenology.gis$DEM_MEAN.SLOPE14)
model14<-lmer(daynum.tenth~DEM_MEAN.SLOPE14+year+(1|site),phenology.gis)
anova(model9,model14)


##########   have shown that simplest model is model 9 #########


############################### plotting model9 ###################################


with(phenology.gis, boxplot(daynum.tenth~DEM_MEAN.SLOPE9,xlab="Site type",ylab="daynum.tenth",ylim = c(0, 200)))
#plot boxplot of model9

library(plyr)
bargraph<-ddply(phenology.gis, c("DEM_MEAN.SLOPE9"),summarise,
                N=length(daynum.tenth),
                mean=mean(daynum.tenth),
                sd=sd(daynum.tenth),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)

ggplot(bargraph, aes(x = DEM_MEAN.SLOPE9, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=0.1)) +
  ggtitle("daynum.tenth vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.tenth") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

p<-ggplot(phenology.gis,aes(x=DEM_MEAN.SLOPE9,y=daynum.tenth))+geom_violin(fill="grey",col="black")+theme_minimal()
p + geom_boxplot(width=0.1) + expand_limits(y=c(0,200)) + scale_y_continuous(breaks=seq(0, 200, 20))+ggtitle("daynum.tenth vs sight Type")+xlab("Site Category")+ylab("daynum.tenth")

test<-ddply(phenology.gis, c("DEM_MEAN.SLOPE9"),summarise,
            N=length(daynum.tenth),
            mean=mean(daynum.tenth),
            sd=sd(daynum.tenth),
            se=sd/ sqrt(N),
            min = min(daynum.tenth),
            max=max(daynum.tenth))
head(test)

