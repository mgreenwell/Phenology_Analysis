search()
ls()
gis.data<-read.table("Landscape_Phenology_Analysis/Data/BBS.CBC.UKBMS.complete.landcover.data.all.scales.soil.dem.configuration.txt",header=T)
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

hist(phenology.gis$LCper,breaks=40,col="red",ylim =c(0,6000),xlim =c(0,100), xlab = "Percentage of calcarious grassland cover",main="Histogram of LC%")
summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(3.108)]<-"low lc"  
phenology.gis$LC.label[phenology.gis$LCper>(3.108)]<-"high lc"  
table(phenology.gis$LC.label)

hist(phenology.gis$DEM_MEAN,breaks=40,col="red",ylim =c(0,1500), xlab = "mean hight above sealevel (m)",main="Histogram of hight above sealevel")
summary(phenology.gis$DEM_MEAN)
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN<(85.76)]<-"low altitude"  #using mean
phenology.gis$DEM_MEAN.label[phenology.gis$DEM_MEAN>(85.76)]<-"high altitude"  
table(phenology.gis$DEM_MEAN.label)


############################ assigning labels ######################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.DEM[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"DEM_MEAN.label"],sep=".")
}
table(phenology.gis$LC.DEM)
names(phenology.gis)

library(lme4)
model<-lmer(daynum.range~LC.DEM+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.range~LC.DEM))


############################## Testing models #############################


modelnull<-lmer(daynum.range~year+(1|site),phenology.gis)
anova(model,modelnull)
#Testing whether LC.DEM has an effect on daynum.range by comparing with a model that just looks at daynum.range vs year with site as random effect
#result shows sig dif between models.
#AIC lower on original model therefore more of results explained by model.

library(plyr)
bargraph<-ddply(phenology.gis, c("LC.DEM"),summarise,
                N=length(daynum.range),
                mean=mean(daynum.range),
                sd=sd(daynum.range),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = LC.DEM, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=4)) +
  ggtitle("daynum.range vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.range") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

phenology.gis$LC.DEM2<-phenology.gis$LC.DEM
phenology.gis$LC.DEM2<-as.factor(phenology.gis$LC.DEM2)
levels(phenology.gis$LC.DEM2)
levels(phenology.gis$LC.DEM2)<-c("High LC Low Alt","All other","All other","All other")
levels(phenology.gis$LC.DEM2)
model2<-lmer(daynum.range~LC.DEM2+year+(1|site),phenology.gis)
anova(model,model2)

#################

phenology.gis$LC.DEM2<-phenology.gis$LC.DEM
phenology.gis$LC.DEM2<-as.factor(phenology.gis$LC.DEM2)
levels(phenology.gis$LC.DEM2)
levels(phenology.gis$LC.DEM2)<-c("A","B","C","A")
levels(phenology.gis$LC.DEM2)
model2<-lmer(daynum.range~LC.DEM2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$LC.DEM3<-phenology.gis$LC.DEM
phenology.gis$LC.DEM3<-as.factor(phenology.gis$LC.DEM3)
levels(phenology.gis$LC.DEM3)
levels(phenology.gis$LC.DEM3)<-c("A","B","A","C")
levels(phenology.gis$LC.DEM3)
model3<-lmer(daynum.range~LC.DEM3+year+(1|site),phenology.gis)
anova(model,model3)

#######

phenology.gis$LC.DEM4<-phenology.gis$LC.DEM
phenology.gis$LC.DEM4<-as.factor(phenology.gis$LC.DEM4)
levels(phenology.gis$LC.DEM4)
levels(phenology.gis$LC.DEM4)<-c("A","A","B","C")
levels(phenology.gis$LC.DEM4)
model4<-lmer(daynum.range~LC.DEM4+year+(1|site),phenology.gis)
anova(model,model4)

#######

phenology.gis$LC.DEM5<-phenology.gis$LC.DEM
phenology.gis$LC.DEM5<-as.factor(phenology.gis$LC.DEM5)
levels(phenology.gis$LC.DEM5)
levels(phenology.gis$LC.DEM5)<-c("B","C","A","A")
levels(phenology.gis$LC.DEM5)
model5<-lmer(daynum.range~LC.DEM5+year+(1|site),phenology.gis)
anova(model,model5)

#######

phenology.gis$LC.DEM6<-phenology.gis$LC.DEM
phenology.gis$LC.DEM6<-as.factor(phenology.gis$LC.DEM6)
levels(phenology.gis$LC.DEM6)
levels(phenology.gis$LC.DEM6)<-c("B","A","C","A")
levels(phenology.gis$LC.DEM6)
model6<-lmer(daynum.range~LC.DEM6+year+(1|site),phenology.gis)
anova(model5,model6)

#######

phenology.gis$LC.DEM7<-phenology.gis$LC.DEM
phenology.gis$LC.DEM7<-as.factor(phenology.gis$LC.DEM7)
levels(phenology.gis$LC.DEM7)
levels(phenology.gis$LC.DEM7)<-c("B","A","A","C")
levels(phenology.gis$LC.DEM7)
model7<-lmer(daynum.range~LC.DEM7+year+(1|site),phenology.gis)
anova(model5,model7)

#######

phenology.gis$LC.DEM8<-phenology.gis$LC.DEM
phenology.gis$LC.DEM8<-as.factor(phenology.gis$LC.DEM8)
levels(phenology.gis$LC.DEM8)
levels(phenology.gis$LC.DEM8)<-c("A","A","B","B")
levels(phenology.gis$LC.DEM8)
model8<-lmer(daynum.range~LC.DEM8+year+(1|site),phenology.gis)
anova(model5,model8)

#######

phenology.gis$LC.DEM9<-phenology.gis$LC.DEM
phenology.gis$LC.DEM9<-as.factor(phenology.gis$LC.DEM9)
levels(phenology.gis$LC.DEM9)
levels(phenology.gis$LC.DEM9)<-c("A","B","A","B")
levels(phenology.gis$LC.DEM9)
model9<-lmer(daynum.range~LC.DEM9+year+(1|site),phenology.gis)
anova(model5,model9)

#######

phenology.gis$LC.DEM10<-phenology.gis$LC.DEM
phenology.gis$LC.DEM10<-as.factor(phenology.gis$LC.DEM10)
levels(phenology.gis$LC.DEM10)
levels(phenology.gis$LC.DEM10)<-c("A","B","B","A")
levels(phenology.gis$LC.DEM10)
model10<-lmer(daynum.range~LC.DEM10+year+(1|site),phenology.gis)
anova(model5,model10)


#######

phenology.gis$LC.DEM11<-phenology.gis$LC.DEM
phenology.gis$LC.DEM11<-as.factor(phenology.gis$LC.DEM11)
levels(phenology.gis$LC.DEM11)
levels(phenology.gis$LC.DEM11)<-c("A","A","A","B")
levels(phenology.gis$LC.DEM11)
model11<-lmer(daynum.range~LC.DEM11+year+(1|site),phenology.gis)
anova(model5,model11)

#######

phenology.gis$LC.DEM12<-phenology.gis$LC.DEM
phenology.gis$LC.DEM12<-as.factor(phenology.gis$LC.DEM12)
levels(phenology.gis$LC.DEM12)
levels(phenology.gis$LC.DEM12)<-c("A","A","B","A")
levels(phenology.gis$LC.DEM12)
model12<-lmer(daynum.range~LC.DEM12+year+(1|site),phenology.gis)
anova(model5,model12)

#######

phenology.gis$LC.DEM13<-phenology.gis$LC.DEM
phenology.gis$LC.DEM13<-as.factor(phenology.gis$LC.DEM13)
levels(phenology.gis$LC.DEM13)
levels(phenology.gis$LC.DEM13)<-c("A","B","A","A")
levels(phenology.gis$LC.DEM13)
model13<-lmer(daynum.range~LC.DEM13+year+(1|site),phenology.gis)
anova(model5,model13)

#######

phenology.gis$LC.DEM14<-phenology.gis$LC.DEM
phenology.gis$LC.DEM14<-as.factor(phenology.gis$LC.DEM14)
levels(phenology.gis$LC.DEM14)
levels(phenology.gis$LC.DEM14)<-c("B","A","A","A")        ###("High LC Low Alt","All other","All other","All other")
levels(phenology.gis$LC.DEM14)
model14<-lmer(daynum.range~LC.DEM14+year+(1|site),phenology.gis)
anova(model5,model14)

##########   have shown that simplest model is model 14 #########


############################### plotting model14 ###################################


with(phenology.gis, boxplot(daynum.range~LC.DEM14,xlab="Site type",ylab="daynum.range"))
#plot boxplot of model2

library(plyr)
bargraph<-ddply(phenology.gis, c("LC.DEM14"),summarise,
                N=length(daynum.range),
                mean=mean(daynum.range),
                sd=sd(daynum.range),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = LC.DEM14, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=0.1)) +
  ggtitle("daynum.range vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.range") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

p<-ggplot(phenology.gis,aes(x=LC.DEM14,y=daynum.range))+geom_violin(fill="grey",col="black")+theme_minimal()
p + geom_boxplot(width=0.1) + expand_limits(y=c(0,200)) + scale_y_continuous(breaks=seq(0, 200, 20))+ggtitle("daynum.range vs sight Type")+xlab("Site Category")+ylab("daynum.range")

test<-ddply(phenology.gis, c("LC.DEM14"),summarise,
            N=length(daynum.range),
            mean=mean(daynum.range),
            sd=sd(daynum.range),
            se=sd/ sqrt(N),
            min = min(daynum.range),
            max=max(daynum.range))
head(test)

