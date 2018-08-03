butt.occ<-read.csv("C:\\Users\\Matt\\Documents\\PhD\\BMS_Data_Analysis\\Loops_Practice\\butterfly_annual_occ_summary_from g.powney_4.11.16 (1).csv",header=T)
head(butt.occ)
# file sent by Gary Powney on 4th Nov 2016. Pers comm: for UKBMS reports species:years with Rhat > 1.1 are dropped, or with sd > 0.2

nrow(butt.occ)
butt.occ<-butt.occ[butt.occ$Rhat<1.1&butt.occ$sd<0.2,]  #  loses only 15 datapoints
#removal of rhat values greater than 1.1 and sd above o.2

results<-NULL
head(butt.occ)
# par(mfrow=c(3,3))
for (i in sort(unique(butt.occ$spp))[]){    # loop for each species

spp.occ<-butt.occ[butt.occ$spp==i,]
#sp.occ - butt.occ where spp collumn=i
head(spp.occ)
spp.occ<-spp.occ[order(spp.occ$year),]
#spp.occ -spp.occ ordered by year
with(spp.occ,plot(mean~year,type="l",lwd=2,main=i))
with(spp.occ,points(X97.5.~year,type="l",lty=2))
with(spp.occ,points(X2.5.~year,type="l",lty=2))
abline(v=1976,col=2)
#plot graph with CIs

                                                                                                    
# Analysis A: Was 1976 a (negative) extreme event for the species?

spp.occ$mean.next.year<-NA
#within spp.occ create column with mean.next.year with values NA
for (y in spp.occ$year){            # loop for each year 
#why is this y?
if(length(spp.occ$mean[spp.occ$year==(y+1)])>0){
spp.occ$mean.next.year[spp.occ$year==y]<-spp.occ$mean[spp.occ$year==(y+1)]
#if legnth is greater than zero give value of previous years mean
} else{ spp.occ$mean.next.year[spp.occ$year==y]<-NA  } # end ifelse loop
} # end y in spp.occ$year

spp.occ$int.ann.chng<-spp.occ$mean.next.year-spp.occ$mean      # interannual change from current year to next
#new column- mean within mean.next etc. within spp.occ
hist(spp.occ$int.ann.chng,breaks=20)
abline(v=spp.occ$int.ann.chng[spp.occ$year==1976],col=2)
sd.extreme.1976<-spp.occ$int.ann.chng[spp.occ$year==1976]/sd(spp.occ$int.ann.chng,na.rm=T)  # this asks how many standard deviations the interannual change in 1976 was from the average interannual change


# Analysis B: Is the mean after 76 lower than before?
pre.76.mean<-mean(spp.occ$mean[spp.occ$year<=1976])
pre.76.se<-sd(spp.occ$mean[spp.occ$year<=1976])/sqrt(length(spp.occ$mean[spp.occ$year<=1976]))

post.76.mean<-mean(spp.occ$mean[spp.occ$year>1976])
post.76.se<-sd(spp.occ$mean[spp.occ$year>1976])/sqrt(length(spp.occ$mean[spp.occ$year>1976]))

results.temp<-data.frame(i,pre.76.mean,pre.76.se,post.76.mean,post.76.se,sd.extreme.1976)
results<-rbind(results,results.temp)
} # end i in species loop


results
hist(results$sd.extreme.1976)   ; abline(v=(-2),col=2) ; abline(v=(2),col=2)# highly significanly
results$mean.diff<- results$post.76.mean-results$pre.76.mean
hist(results$mean.diff,breaks=15) ; abline(v=0)

with(results,plot(sd.extreme.1976~mean.diff))
abline(h=0,lty=2) ; abline(h=(-2),lty=3)
summary(lm(sd.extreme.1976~mean.diff,results))

# to do: calculate whether there are significant differences in pre and post mean


