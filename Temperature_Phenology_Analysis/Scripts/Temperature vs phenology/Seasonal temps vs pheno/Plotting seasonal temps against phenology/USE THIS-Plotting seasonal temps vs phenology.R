library(tidyverse)


phenology.temperature<-read.csv("Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature.csv", header = TRUE)
head(phenology.temperature)
phenology.temperature.fourway<-read.csv("Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature_for_fourway_plot.csv", header = TRUE)

phenology.temperature.individuals<-read.csv("Landscape_Phenology_Analysis/Outputs/Temperature_vs_Phenology/merged_data_set_of_phenology_and_temperature_for_individual_plots.csv", header = TRUE)




############################################quad plot#######################################
#plots single window with four plots. Each plot has all four phenological aspects

graphs<-ggplot(phenology.temperature, 
               aes(x=temperature, y=length.date, colour = flight.period, shape = flight.period)) + 
  geom_point(shape=16) + 
  geom_smooth(method=lm) + 
  ylab("Day/days(range)") + 
  xlab("Temperature ?C") + 
  theme_bw()+
  theme(legend.position="bottom") +
          scale_colour_discrete(name="Flight\nPeriod")
graphs<-graphs + 
  facet_wrap(~season,scales='free') + 
  theme(strip.text = element_text(size = 15)) +
  theme(axis.title = element_text (,,,20)) +
  theme(axis.text = element_text(,,,12)) +
theme(legend.text = element_text(,,,15)) +
  theme(legend.title = element_text(,,,15))
graphs
theme(strip.text.x = element_text(size = 8, colour = "orange", angle = 90))
#####################################fourway plots###########################

#add yearly means columns 
head(phenology.temperature.fourway)
phenology.temperature.fourway$AV = apply(phenology.temperature.fourway[,c(4:7)],1,mean)
head(phenology.temperature.fourway)


head(phenology.temperature.individuals)
phenology.temperature.individuals$AV = apply(phenology.temperature.individuals[,c(6:9)],1,mean)
head(phenology.temperature.individuals)

################

#plots 1 plot per season with all 4 aspects of phenology

#yearly average
year.plot <- ggplot(phenology.temperature.fourway, 
                    aes(x = AV, y = length.date, shape = flight.period, colour = flight.period)) +
  geom_point(shape = 16) +
  geom_smooth(method = lm) + 
  ylab("Day/days(range)") + 
  xlab("Temperature ?C") + 
   theme_bw() +
  theme(axis.title = element_text (,,,15)) +
  theme(axis.text = element_text(,,,12)) +
  theme(legend.text = element_text(,,,15)) +
  theme(legend.title = element_text(,,,15)) +
  theme(legend.position="bottom") +
  scale_colour_discrete(name="Flight\nPeriod",
                         breaks=c("average.mean", "average.nintieth", "average.range", "average.tenth"),
                         labels=c("Mean Day", "End Day", "Flight Period Length", "Start Day"))
year.plot


#Spring
spring.plot<-ggplot(phenology.temperature.fourway, aes(x=MAM, y=length.date,shape=flight.period, color=flight.period)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("Spring temperatures vs phenology")+theme(legend.title=element_blank())
spring.plot

#Summer
summer.plot<-ggplot(phenology.temperature.fourway, aes(x=JJA, y=length.date,shape=flight.period, color=flight.period)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("Summer temperatures vs phenology")+theme(legend.title=element_blank())
summer.plot

#Autumn
autumn.plot<-ggplot(phenology.temperature.fourway, aes(x=SON, y=length.date,shape=flight.period, color=flight.period)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("Autumn temperatures vs phenology")+theme(legend.title=element_blank())
autumn.plot

#Winter
winter.plot<-ggplot(phenology.temperature.fourway, aes(x=DJF, y=length.date,shape=flight.period, color=flight.period)) +
  geom_point(shape=16) +    
  geom_smooth(method=lm)+ylab("Day/days(range)")+xlab("Temperature ?C")+ ggtitle("Winter temperatures vs phenology")+theme(legend.title=element_blank())
winter.plot


#############################################################################################
#Individual plots for running models

###year plots
#tenth
#year.tenth<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.tenth)) +
# geom_point(shape=1) + ggtitle("year temperatures vs start day") +
#geom_smooth(method=lm)+ylab("Day")+xlab("year Temperature ?C")
#year.tenth

#nintieth
#year.nintieth<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.nintieth)) +
#geom_point(shape=1) + ggtitle("year temperatures vs end day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("year Temperature ?C")
#year.nintieth

#mean
#year.mean<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.mean)) +
#geom_point(shape=1) + ggtitle("year temperatures vs mean day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("year Temperature ?C")
#year.mean

#range
#year.range<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.range)) +
#geom_point(shape=1) + ggtitle("year temperatures vs flight period") +
# geom_smooth(method=lm)+ylab("Days")+xlab("year Temperature ?C")
#year.range

###spring plots
#tenth
#spring.tenth<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.tenth)) +
#geom_point(shape=1) + ggtitle("Spring temperatures vs start day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Spring Temperature ?C")
#spring.tenth

#nintieth
#spring.nintieth<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.nintieth)) +
# geom_point(shape=1) + ggtitle("Spring temperatures vs end day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Spring Temperature ?C")
#spring.nintieth

#mean
#spring.mean<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.mean)) +
#geom_point(shape=1) + ggtitle("Spring temperatures vs mean day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Spring Temperature ?C")
#spring.mean

#range
#spring.range<-ggplot(phenology.temperature.individuals, aes(x=MAM, y=average.range)) +
#geom_point(shape=1) + ggtitle("Spring temperatures vs flight period") +
# geom_smooth(method=lm)+ylab("Days")+xlab("spring Temperature ?C")
#spring.range


####summer temperatures
#tenth
#summer.tenth<-ggplot(phenology.temperature.individuals, aes(x=JJA, y=average.tenth)) +
#geom_point(shape=1) + ggtitle("Summer temperatures vs start day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Summer Temperature ?C")
#summer.tenth

#nintieth
#summer.nintieth<-ggplot(phenology.temperature.individuals, aes(x=JJA, y=average.nintieth)) +
#geom_point(shape=1) + ggtitle("Summer temperatures vs end day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Summer Temperature ?C")
#summer.nintieth

#mean
#summer.mean<-ggplot(phenology.temperature.individuals, aes(x=JJA, y=average.mean)) +
#geom_point(shape=1) + ggtitle("Summer temperatures vs mean day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Summer Temperature ?C")
#summer.mean

#range
#summer.range<-ggplot(phenology.temperature.individuals, aes(x=JJA, y=average.range)) +
#geom_point(shape=1) + ggtitle("Summer temperatures vs flight period") +
# geom_smooth(method=lm)+ylab("Days")+xlab("Summer Temperature ?C")
#summer.range


####autumn temperatures
#tenth
#autumn.tenth<-ggplot(phenology.temperature.individuals, aes(x=SON, y=average.tenth)) +
#geom_point(shape=1) + ggtitle("Autumn temperatures vs start day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Autumn Temperature ?C")
#autumn.tenth

#nintieth
#autumn.nintieth<-ggplot(phenology.temperature.individuals, aes(x=SON, y=average.nintieth)) +
#geom_point(shape=1) + ggtitle("Autumn temperatures vs end day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Autumn Temperature ?C")
#autumn.nintieth

#mean
#autumn.mean<-ggplot(phenology.temperature.individuals, aes(x=SON, y=average.mean)) +
#geom_point(shape=1) + ggtitle("Autumn temperatures vs mean day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Autumn Temperature ?C")
#autumn.mean

#range
#autumn.range<-ggplot(phenology.temperature.individuals, aes(x=SON, y=average.range)) +
# geom_point(shape=1) + ggtitle("Autumn temperatures vs flight period") +
#geom_smooth(method=lm)+ylab("Days")+xlab("Autumn Temperature ?C")
#autumn.range

#winter temperatures
#tenth
#winter.tenth<-ggplot(phenology.temperature.individuals, aes(x=DJF, y=average.tenth)) +
#geom_point(shape=1) + ggtitle("Winter temperatures vs start day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Winter Temperature ?C")
#winter.tenth

#nintieth
#winter.nintieth<-ggplot(phenology.temperature.individuals, aes(x=DJF, y=average.nintieth)) +
# geom_point(shape=1) + ggtitle("Winter temperatures vs end day") +
#geom_smooth(method=lm)+ylab("Day")+xlab("Winter Temperature ?C")
#winter.nintieth

#mean
#winter.mean<-ggplot(phenology.temperature.individuals, aes(x=DJF, y=average.mean)) +
#geom_point(shape=1) + ggtitle("Winter temperatures vs mean day") +
# geom_smooth(method=lm)+ylab("Day")+xlab("Winter Temperature ?C")
#winter.mean

#range
#winter.range<-ggplot(phenology.temperature.individuals, aes(x=DJF, y=average.range)) +
# geom_point(shape=1) + ggtitle("Winter temperatures vs flight period") +
#geom_smooth(method=lm)+ylab("Days")+xlab("Winter Temperature ?C")
#winter.range


####### models ######

head(phenology.temperature.individuals)
#year

year.tenth<-lm(average.tenth~AV,data=phenology.temperature.individuals)
summary(year.tenth)

year.nintieth<-lm(average.nintieth~AV,data=phenology.temperature.individuals)
summary(year.nintieth)

year.mean<-lm(average.mean ~ AV,data=phenology.temperature.individuals)
summary(year.mean)

year.range<-lm(average.range ~ AV, data=phenology.temperature.individuals)
summary(year.range)

#spring

spring.tenth<-lm(average.tenth ~ MAM,data=phenology.temperature.individuals)
summary(spring.tenth)

spring.nintieth<-lm(average.nintieth ~ MAM,data=phenology.temperature.individuals)
summary(spring.nintieth)

spring.mean<-lm(average.mean ~ MAM,data=phenology.temperature.individuals)
summary(spring.mean)

spring.range<-lm(average.range ~ MAM, data=phenology.temperature.individuals)
summary(spring.range)

#summer

summer.tenth<-lm(average.tenth ~ JJA,data=phenology.temperature.individuals)
summary(summer.tenth)

summer.nintieth<-lm(average.nintieth ~ JJA,data=phenology.temperature.individuals)
summary(summer.nintieth)

summer.mean<-lm(average.mean ~ JJA,data=phenology.temperature.individuals)
summary(summer.mean)

summer.range<-lm(average.range ~ JJA, data=phenology.temperature.individuals)
summary(summer.range)

#autumn

autumn.tenth<-lm(average.tenth ~ SON,data=phenology.temperature.individuals)
summary(autumn.tenth)

autumn.nintieth<-lm(average.nintieth ~ SON,data=phenology.temperature.individuals)
summary(autumn.nintieth)

autumn.mean<-lm(average.mean ~ SON,data=phenology.temperature.individuals)
summary(autumn.mean)

autumn.range<-lm(average.range ~ SON, data=phenology.temperature.individuals)
summary(autumn.range)

#winter

winter.tenth<-lm(average.tenth ~ DJF,data=phenology.temperature.individuals)
summary(winter.tenth)

winter.nintieth<-lm(average.nintieth ~ DJF,data=phenology.temperature.individuals)
summary(winter.nintieth)

winter.mean<-lm(average.mean ~ DJF,data=phenology.temperature.individuals)
summary(winter.mean)

winter.range<-lm(average.range ~ DJF, data=phenology.temperature.individuals)
summary(winter.range)

