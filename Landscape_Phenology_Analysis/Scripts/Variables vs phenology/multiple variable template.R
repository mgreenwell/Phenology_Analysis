phenology.gis$LC.SLOPE2<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE2<-as.factor(phenology.gis$LC.SLOPE2)
levels(phenology.gis$LC.SLOPE2)
levels(phenology.gis$LC.SLOPE2)<-c("A","B","C","A")
levels(phenology.gis$LC.SLOPE2)
model2<-lmer(daynum.mean~LC.SLOPE2+year+(1|site),phenology.gis)
anova(model,model2)

###### 

phenology.gis$LC.SLOPE3<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE3<-as.factor(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)
levels(phenology.gis$LC.SLOPE3)<-c("A","B","A","C")
levels(phenology.gis$LC.SLOPE3)
model3<-lmer(daynum.mean~LC.SLOPE3+year+(1|site),phenology.gis)
anova(model,model3)

#######

phenology.gis$LC.SLOPE4<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE4<-as.factor(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)
levels(phenology.gis$LC.SLOPE4)<-c("A","A","B","C")
levels(phenology.gis$LC.SLOPE4)
model4<-lmer(daynum.mean~LC.SLOPE4+year+(1|site),phenology.gis)
anova(model,model4)

#######

phenology.gis$LC.SLOPE5<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE5<-as.factor(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)
levels(phenology.gis$LC.SLOPE5)<-c("B","C","A","A")
levels(phenology.gis$LC.SLOPE5)
model5<-lmer(daynum.mean~LC.SLOPE5+year+(1|site),phenology.gis)
anova(model,model5)

#######

phenology.gis$LC.SLOPE6<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE6<-as.factor(phenology.gis$LC.SLOPE6)
levels(phenology.gis$LC.SLOPE6)
levels(phenology.gis$LC.SLOPE6)<-c("B","A","C","A")
levels(phenology.gis$LC.SLOPE6)
model6<-lmer(daynum.mean~LC.SLOPE6+year+(1|site),phenology.gis)
anova(model,model6)

#######

phenology.gis$LC.SLOPE7<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE7<-as.factor(phenology.gis$LC.SLOPE7)
levels(phenology.gis$LC.SLOPE7)
levels(phenology.gis$LC.SLOPE7)<-c("B","A","A","C")
levels(phenology.gis$LC.SLOPE7)
model7<-lmer(daynum.mean~LC.SLOPE7+year+(1|site),phenology.gis)
anova(model,model7)

#######

phenology.gis$LC.SLOPE8<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE8<-as.factor(phenology.gis$LC.SLOPE8)
levels(phenology.gis$LC.SLOPE8)
levels(phenology.gis$LC.SLOPE8)<-c("A","A","B","B")
levels(phenology.gis$LC.SLOPE8)
model8<-lmer(daynum.mean~LC.SLOPE8+year+(1|site),phenology.gis)
anova(model,model8)

#######

phenology.gis$LC.SLOPE9<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE9<-as.factor(phenology.gis$LC.SLOPE9)
levels(phenology.gis$LC.SLOPE9)
levels(phenology.gis$LC.SLOPE9)<-c("A","B","A","B")
levels(phenology.gis$LC.SLOPE9)
model9<-lmer(daynum.mean~LC.SLOPE9+year+(1|site),phenology.gis)
anova(model,model9)

#######

phenology.gis$LC.SLOPE10<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE10<-as.factor(phenology.gis$LC.SLOPE10)
levels(phenology.gis$LC.SLOPE10)
levels(phenology.gis$LC.SLOPE10)<-c("A","B","B","A")
levels(phenology.gis$LC.SLOPE10)
model10<-lmer(daynum.mean~LC.SLOPE10+year+(1|site),phenology.gis)
anova(model,model10)


#######

phenology.gis$LC.SLOPE11<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE11<-as.factor(phenology.gis$LC.SLOPE11)
levels(phenology.gis$LC.SLOPE11)
levels(phenology.gis$LC.SLOPE11)<-c("A","A","A","B")
levels(phenology.gis$LC.SLOPE11)
model11<-lmer(daynum.mean~LC.SLOPE11+year+(1|site),phenology.gis)
anova(model,model11)

#######

phenology.gis$LC.SLOPE12<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE12<-as.factor(phenology.gis$LC.SLOPE12)
levels(phenology.gis$LC.SLOPE12)
levels(phenology.gis$LC.SLOPE12)<-c("A","A","B","A")
levels(phenology.gis$LC.SLOPE12)
model12<-lmer(daynum.mean~LC.SLOPE12+year+(1|site),phenology.gis)
anova(model,model12)

#######

phenology.gis$LC.SLOPE13<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE13<-as.factor(phenology.gis$LC.SLOPE13)
levels(phenology.gis$LC.SLOPE13)
levels(phenology.gis$LC.SLOPE13)<-c("A","B","A","A")
levels(phenology.gis$LC.SLOPE13)
model13<-lmer(daynum.mean~LC.SLOPE13+year+(1|site),phenology.gis)
anova(model,model13)

#######

phenology.gis$LC.SLOPE14<-phenology.gis$LC.SLOPE
phenology.gis$LC.SLOPE14<-as.factor(phenology.gis$LC.SLOPE14)
levels(phenology.gis$LC.SLOPE14)
levels(phenology.gis$LC.SLOPE14)<-c("B","A","A","A")
levels(phenology.gis$LC.SLOPE14)
model14<-lmer(daynum.mean~LC.SLOPE14+year+(1|site),phenology.gis)
anova(model,model14)