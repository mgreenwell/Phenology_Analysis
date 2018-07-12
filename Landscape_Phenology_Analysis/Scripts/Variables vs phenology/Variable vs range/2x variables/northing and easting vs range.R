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

summary(phenology.gis$POINT_Y)
phenology.gis$POINT_Y.label[phenology.gis$POINT_Y<(252700)]<-"western"  #using mean
phenology.gis$POINT_Y.label[phenology.gis$POINT_Y>(252700)]<-"eastern"  
table(phenology.gis$POINT_Y.label)


#head(phenology.gis)



############################ assigning labels ######################################


for (i in 1:nrow(phenology.gis)){
  phenology.gis$POINT_X.POINT_Y[i]<-paste(phenology.gis[i,"POINT_X.label"],phenology.gis[i,"POINT_Y.label"],sep=".")
}
table(phenology.gis$POINT_X.POINT_Y)
names(phenology.gis)

library(lme4)
model<-lmer(daynum.range~POINT_X.POINT_Y+year+(1|site),phenology.gis)

# (1|site) gives site as random effect- not a factor of interest
summary(model)
names(phenology.gis)
par(mfrow=c(1,1))
with(phenology.gis, boxplot(daynum.range~POINT_X.POINT_Y))


############################## Testing models #############################


modelnull<-lmer(daynum.range~year+(1|site),phenology.gis)
anova(model,modelnull)
#Testing whether POINT_X.POINT_Y has an effect on daynum.range by comparing with a model that just looks at daynum.range vs year with site as random effect
#result shows sig dif between models.
#AIC lower on original model therefore more of results explained by model.


library(plyr)
bargraph<-ddply(phenology.gis, c("POINT_X.POINT_Y"),summarise,
                N=length(daynum.range),
                mean=mean(daynum.range),
                sd=sd(daynum.range),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = POINT_X.POINT_Y, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=4)) +
  ggtitle("daynum.range vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.range") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))


phenology.gis$POINT_X.POINT_Y2<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y2<-as.factor(phenology.gis$POINT_X.POINT_Y2)
levels(phenology.gis$POINT_X.POINT_Y2)
levels(phenology.gis$POINT_X.POINT_Y2)<-c("northern.easterm & southern.eastern","northern.western & southern.western","northern.easterm & southern.eastern","northern.western & southern.western")
levels(phenology.gis$POINT_X.POINT_Y2)
model2<-lmer(daynum.range~POINT_X.POINT_Y2+year+(1|site),phenology.gis)
anova(model,model2)

#model 2 better fit

phenology.gis$POINT_X.POINT_Y2<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y2<-as.factor(phenology.gis$POINT_X.POINT_Y2)
levels(phenology.gis$POINT_X.POINT_Y2)
levels(phenology.gis$POINT_X.POINT_Y2)<-c("A","B","C","A")
levels(phenology.gis$POINT_X.POINT_Y2)
model2<-lmer(daynum.range~POINT_X.POINT_Y2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$POINT_X.POINT_Y3<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y3<-as.factor(phenology.gis$POINT_X.POINT_Y3)
levels(phenology.gis$POINT_X.POINT_Y3)
levels(phenology.gis$POINT_X.POINT_Y3)<-c("A","B","A","C")
levels(phenology.gis$POINT_X.POINT_Y3)
model3<-lmer(daynum.range~POINT_X.POINT_Y3+year+(1|site),phenology.gis)
anova(model,model3)

#######

phenology.gis$POINT_X.POINT_Y4<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y4<-as.factor(phenology.gis$POINT_X.POINT_Y4)
levels(phenology.gis$POINT_X.POINT_Y4)
levels(phenology.gis$POINT_X.POINT_Y4)<-c("A","A","B","C")
levels(phenology.gis$POINT_X.POINT_Y4)
model4<-lmer(daynum.range~POINT_X.POINT_Y4+year+(1|site),phenology.gis)
anova(model3,model4)

#######

phenology.gis$POINT_X.POINT_Y5<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y5<-as.factor(phenology.gis$POINT_X.POINT_Y5)
levels(phenology.gis$POINT_X.POINT_Y5)
levels(phenology.gis$POINT_X.POINT_Y5)<-c("B","C","A","A")
levels(phenology.gis$POINT_X.POINT_Y5)
model5<-lmer(daynum.range~POINT_X.POINT_Y5+year+(1|site),phenology.gis)
anova(model3,model5)

#######

phenology.gis$POINT_X.POINT_Y6<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y6<-as.factor(phenology.gis$POINT_X.POINT_Y6)
levels(phenology.gis$POINT_X.POINT_Y6)
levels(phenology.gis$POINT_X.POINT_Y6)<-c("B","A","C","A")
levels(phenology.gis$POINT_X.POINT_Y6)
model6<-lmer(daynum.range~POINT_X.POINT_Y6+year+(1|site),phenology.gis)
anova(model3,model6)

#######

phenology.gis$POINT_X.POINT_Y7<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y7<-as.factor(phenology.gis$POINT_X.POINT_Y7)
levels(phenology.gis$POINT_X.POINT_Y7)
levels(phenology.gis$POINT_X.POINT_Y7)<-c("B","A","A","C")
levels(phenology.gis$POINT_X.POINT_Y7)
model7<-lmer(daynum.range~POINT_X.POINT_Y7+year+(1|site),phenology.gis)
anova(model3,model7)

#######

phenology.gis$POINT_X.POINT_Y8<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y8<-as.factor(phenology.gis$POINT_X.POINT_Y8)
levels(phenology.gis$POINT_X.POINT_Y8)
levels(phenology.gis$POINT_X.POINT_Y8)<-c("A","A","B","B")
levels(phenology.gis$POINT_X.POINT_Y8)
model8<-lmer(daynum.range~POINT_X.POINT_Y8+year+(1|site),phenology.gis)
anova(model3,model8)

#######

phenology.gis$POINT_X.POINT_Y9<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y9<-as.factor(phenology.gis$POINT_X.POINT_Y9)
levels(phenology.gis$POINT_X.POINT_Y9)
levels(phenology.gis$POINT_X.POINT_Y9)<-c("A","B","A","B")
levels(phenology.gis$POINT_X.POINT_Y9)
model9<-lmer(daynum.range~POINT_X.POINT_Y9+year+(1|site),phenology.gis)
anova(model3,model9)

#######

phenology.gis$POINT_X.POINT_Y10<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y10<-as.factor(phenology.gis$POINT_X.POINT_Y10)
levels(phenology.gis$POINT_X.POINT_Y10)
levels(phenology.gis$POINT_X.POINT_Y10)<-c("A","B","B","A")
levels(phenology.gis$POINT_X.POINT_Y10)
model10<-lmer(daynum.range~POINT_X.POINT_Y10+year+(1|site),phenology.gis)
anova(model9,model10)


#######

phenology.gis$POINT_X.POINT_Y11<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y11<-as.factor(phenology.gis$POINT_X.POINT_Y11)
levels(phenology.gis$POINT_X.POINT_Y11)
levels(phenology.gis$POINT_X.POINT_Y11)<-c("A","A","A","B")
levels(phenology.gis$POINT_X.POINT_Y11)
model11<-lmer(daynum.range~POINT_X.POINT_Y11+year+(1|site),phenology.gis)
anova(model9,model11)

#######

phenology.gis$POINT_X.POINT_Y12<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y12<-as.factor(phenology.gis$POINT_X.POINT_Y12)
levels(phenology.gis$POINT_X.POINT_Y12)
levels(phenology.gis$POINT_X.POINT_Y12)<-c("A","A","B","A")
levels(phenology.gis$POINT_X.POINT_Y12)
model12<-lmer(daynum.range~POINT_X.POINT_Y12+year+(1|site),phenology.gis)
anova(model9,model12)

#######

phenology.gis$POINT_X.POINT_Y13<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y13<-as.factor(phenology.gis$POINT_X.POINT_Y13)
levels(phenology.gis$POINT_X.POINT_Y13)
levels(phenology.gis$POINT_X.POINT_Y13)<-c("A","B","A","A")
levels(phenology.gis$POINT_X.POINT_Y13)
model13<-lmer(daynum.range~POINT_X.POINT_Y13+year+(1|site),phenology.gis)
anova(model9,model13)

#######

phenology.gis$POINT_X.POINT_Y14<-phenology.gis$POINT_X.POINT_Y
phenology.gis$POINT_X.POINT_Y14<-as.factor(phenology.gis$POINT_X.POINT_Y14)
levels(phenology.gis$POINT_X.POINT_Y14)
levels(phenology.gis$POINT_X.POINT_Y14)<-c("B","A","A","A")
levels(phenology.gis$POINT_X.POINT_Y14)
model14<-lmer(daynum.range~POINT_X.POINT_Y14+year+(1|site),phenology.gis)
anova(model9,model14)

############################### plotting model9 ###################################


with(phenology.gis, boxplot(daynum.range~POINT_X.POINT_Y9,xlab="Site type",ylab="daynum.range",ylim = c(0, 200)))
#plot boxplot of model2

library(plyr)
bargraph<-ddply(phenology.gis, c("POINT_X.POINT_Y9"),summarise,
                N=length(daynum.range),
                mean=mean(daynum.range),
                sd=sd(daynum.range),
                se=sd/ sqrt(N))
head(bargraph)

library(ggplot2)
#??ggplot2
ggplot(bargraph, aes(x = POINT_X.POINT_Y9, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity",width=0.4,col="black",fill="grey") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se,width=0.1)) +
  ggtitle("daynum.range vs sight conditions") + 
  theme_grey() + 
  theme(panel.grid.major = element_blank()) + 
  labs(x="Site category",y="daynum.range") + 
  expand_limits(y=c(0,130)) + scale_y_continuous(breaks=seq(0, 130, 10))

p<-ggplot(phenology.gis,aes(x=POINT_X.POINT_Y9,y=daynum.range))+geom_violin(fill="grey",col="black")+theme_minimal()
p + geom_boxplot(width=0.1) + expand_limits(y=c(0,200)) + scale_y_continuous(breaks=seq(0, 200, 20))+ggtitle("daynum.range vs sight Type")+xlab("Site Category")+ylab("daynum.range")

test<-ddply(phenology.gis, c("POINT_X.POINT_Y9"),summarise,
            N=length(daynum.range),
            mean=mean(daynum.range),
            sd=sd(daynum.range),
            se=sd/ sqrt(N),
            min = min(daynum.range),
            max=max(daynum.range))
head(test)

