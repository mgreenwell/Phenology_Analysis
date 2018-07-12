library(tidyverse)

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

summary(phenology.gis$POINT_X)
phenology.gis$POINT_X.label[phenology.gis$POINT_X<(448100)]<-"southern"  
phenology.gis$POINT_X.label[phenology.gis$POINT_X>(448100)]<-"northern"  
table(phenology.gis$POINT_X.label)

summary(phenology.gis$LCper)
phenology.gis$LC.label[phenology.gis$LCper<(3.108)]<-"low chalk"  
phenology.gis$LC.label[phenology.gis$LCper>(3.108)]<-"high chalk"  
table(phenology.gis$LC.label)

############################ assigning labels ######################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$LC.LAT[i]<-paste(phenology.gis[i,"LC.label"],phenology.gis[i,"POINT_X.label"],sep=".")
}
table(phenology.gis$LC.LAT)
names(phenology.gis)

library(lme4)
model<-lmer(daynum.mean~LC.LAT+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.mean~LC.LAT))

############################## Testing models #############################

library(plyr)
bargraph<-ddply(phenology.gis, c("LC.LAT"),summarise,
                N=length(daynum.mean),
                mean=mean(daynum.mean),
                sd=sd(daynum.mean),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = LC.LAT, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=4)) +
  ggtitle("daynum.mean vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.mean") + 
  expand_limits(y=c(0,140)) + scale_y_continuous(breaks=seq(0, 140, 10))

#########################

phenology.gis$LC.LAT2<-phenology.gis$LC.LAT
phenology.gis$LC.LAT2<-as.factor(phenology.gis$LC.LAT2)
levels(phenology.gis$LC.LAT2)
levels(phenology.gis$LC.LAT2)<-c("A","B","C","A")
levels(phenology.gis$LC.LAT2)
model2<-lmer(daynum.mean~LC.LAT2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$LC.LAT3<-phenology.gis$LC.LAT
phenology.gis$LC.LAT3<-as.factor(phenology.gis$LC.LAT3)
levels(phenology.gis$LC.LAT3)
levels(phenology.gis$LC.LAT3)<-c("A","B","A","C")
levels(phenology.gis$LC.LAT3)
model3<-lmer(daynum.mean~LC.LAT3+year+(1|site),phenology.gis)
anova(model,model3)

#######

phenology.gis$LC.LAT4<-phenology.gis$LC.LAT
phenology.gis$LC.LAT4<-as.factor(phenology.gis$LC.LAT4)
levels(phenology.gis$LC.LAT4)
levels(phenology.gis$LC.LAT4)<-c("A","A","B","C")
levels(phenology.gis$LC.LAT4)
model4<-lmer(daynum.mean~LC.LAT4+year+(1|site),phenology.gis)
anova(model,model4)

#######

phenology.gis$LC.LAT5<-phenology.gis$LC.LAT
phenology.gis$LC.LAT5<-as.factor(phenology.gis$LC.LAT5)
levels(phenology.gis$LC.LAT5)
levels(phenology.gis$LC.LAT5)<-c("B","C","A","A")
levels(phenology.gis$LC.LAT5)
model5<-lmer(daynum.mean~LC.LAT5+year+(1|site),phenology.gis)
anova(model,model5)

#######

phenology.gis$LC.LAT6<-phenology.gis$LC.LAT
phenology.gis$LC.LAT6<-as.factor(phenology.gis$LC.LAT6)
levels(phenology.gis$LC.LAT6)
levels(phenology.gis$LC.LAT6)<-c("B","A","C","A")
levels(phenology.gis$LC.LAT6)
model6<-lmer(daynum.mean~LC.LAT6+year+(1|site),phenology.gis)
anova(model,model6)

#######

phenology.gis$LC.LAT7<-phenology.gis$LC.LAT
phenology.gis$LC.LAT7<-as.factor(phenology.gis$LC.LAT7)
levels(phenology.gis$LC.LAT7)
levels(phenology.gis$LC.LAT7)<-c("B","A","A","C")
levels(phenology.gis$LC.LAT7)
model7<-lmer(daynum.mean~LC.LAT7+year+(1|site),phenology.gis)
anova(model,model7)

#######

phenology.gis$LC.LAT8<-phenology.gis$LC.LAT
phenology.gis$LC.LAT8<-as.factor(phenology.gis$LC.LAT8)
levels(phenology.gis$LC.LAT8)
levels(phenology.gis$LC.LAT8)<-c("A","A","B","B")
levels(phenology.gis$LC.LAT8)
model8<-lmer(daynum.mean~LC.LAT8+year+(1|site),phenology.gis)
anova(model,model8)

#######

phenology.gis$LC.LAT9<-phenology.gis$LC.LAT
phenology.gis$LC.LAT9<-as.factor(phenology.gis$LC.LAT9)
levels(phenology.gis$LC.LAT9)
levels(phenology.gis$LC.LAT9)<-c("A","B","A","B")
levels(phenology.gis$LC.LAT9)
model9<-lmer(daynum.mean~LC.LAT9+year+(1|site),phenology.gis)
anova(model,model9)

#######

phenology.gis$LC.LAT10<-phenology.gis$LC.LAT
phenology.gis$LC.LAT10<-as.factor(phenology.gis$LC.LAT10)
levels(phenology.gis$LC.LAT10)
levels(phenology.gis$LC.LAT10)<-c("A","B","B","A")
levels(phenology.gis$LC.LAT10)
model10<-lmer(daynum.mean~LC.LAT10+year+(1|site),phenology.gis)
anova(model,model10)


#######

phenology.gis$LC.LAT11<-phenology.gis$LC.LAT
phenology.gis$LC.LAT11<-as.factor(phenology.gis$LC.LAT11)
levels(phenology.gis$LC.LAT11)
levels(phenology.gis$LC.LAT11)<-c("A","A","A","B")
levels(phenology.gis$LC.LAT11)
model11<-lmer(daynum.mean~LC.LAT11+year+(1|site),phenology.gis)
anova(model,model11)

#######

phenology.gis$LC.LAT12<-phenology.gis$LC.LAT
phenology.gis$LC.LAT12<-as.factor(phenology.gis$LC.LAT12)
levels(phenology.gis$LC.LAT12)
levels(phenology.gis$LC.LAT12)<-c("A","A","B","A")
levels(phenology.gis$LC.LAT12)
model12<-lmer(daynum.mean~LC.LAT12+year+(1|site),phenology.gis)
anova(model,model12)

#######

phenology.gis$LC.LAT13<-phenology.gis$LC.LAT
phenology.gis$LC.LAT13<-as.factor(phenology.gis$LC.LAT13)
levels(phenology.gis$LC.LAT13)
levels(phenology.gis$LC.LAT13)<-c("A","B","A","A")
levels(phenology.gis$LC.LAT13)
model13<-lmer(daynum.mean~LC.LAT13+year+(1|site),phenology.gis)
anova(model,model13)

#######

phenology.gis$LC.LAT14<-phenology.gis$LC.LAT
phenology.gis$LC.LAT14<-as.factor(phenology.gis$LC.LAT14)
levels(phenology.gis$LC.LAT14)
levels(phenology.gis$LC.LAT14)<-c("B","A","A","A")     #("High chalk northern","Other","Other","Other")
levels(phenology.gis$LC.LAT14)
model14<-lmer(daynum.mean~LC.LAT14+year+(1|site),phenology.gis)
anova(model,model14)

##########   have shown that simplest model is model #########
with(phenology.gis, boxplot(daynum.mean~LC.LAT,xlab="Site type",ylab="Mean flight day"))

summary(LC.LAT)
library(plyr)
bargraph<-ddply(phenology.gis, c("LC.LAT"),summarise,
                N=length(daynum.mean),
                mean=mean(daynum.mean),
                sd=sd(daynum.mean),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = LC.LAT, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=0.1)) +
  ggtitle("daynum.mean vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.mean") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

p<-ggplot(phenology.gis,aes(x=LC.LAT,y=daynum.mean))+geom_violin(fill="grey",col="black")+theme_minimal()
p + geom_boxplot(width=0.1) + expand_limits(y=c(0,200)) + scale_y_continuous(breaks=seq(0, 200, 20))+ggtitle("daynum.mean vs sight Type")+xlab("Site Category")+ylab("daynum.mean")

test<-ddply(phenology.gis, c("LC.LAT"),summarise,
            N=length(daynum.mean),
            mean=mean(daynum.mean),
            sd=sd(daynum.mean),
            se=sd/ sqrt(N),
            min = min(daynum.mean),
            max=max(daynum.mean))
head(test)

