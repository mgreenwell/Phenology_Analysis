head(butt.occ)

butt.occ$spp.2<-"test"
butt.occ$spp.3<-NA
for (i in 1:nrow(butt.occ)){
  butt.occ$spp.3[i]<-paste(butt.occ[i,"spp"],butt.occ[i,"spp.2"],sep=".")
}



# example loop

results<-NULL
par(mfrow=c(3,3))
for (i in sort(unique(butt.occ$spp))[1:9]){    # loop for each species
  
  spp.occ<-butt.occ[butt.occ$spp==i,]
  head(spp.occ)
  spp.occ<-spp.occ[order(spp.occ$year),]
  with(spp.occ,plot(mean~year,type="l",lwd=2,main=i))
  with(spp.occ,points(X97.5.~year,type="l",lty=2))
  with(spp.occ,points(X2.5.~year,type="l",lty=2))
  abline(v=1976,col=2)
  
  model<-lm(mean~year,spp.occ)
  slope.year<-summary(model)$coefficients[2]
  
  
  # Analysis A: Was 1976 a (negative) extreme event for the species?
  spp.occ$int.ann.chng<-NA
  
  for (y in spp.occ$year){            # loop for each year
    if(length(spp.occ$mean[spp.occ$year==(y-1)])>0){
      spp.occ$mean.last.year[spp.occ$year==y]<-spp.occ$mean[spp.occ$year==(y-1)]
    } else{ spp.occ$mean.last.year[spp.occ$year==y]<-NA  } # end ifelse loop
  } # end y in spp.occ$year
  
  
  # Analysis B: Is the mean after 76 lower than before?
  pre.76.mean<-mean(spp.occ$mean[spp.occ$year<=1976])
  pre.76.se<-sd(spp.occ$mean[spp.occ$year<=1976])/sqrt(length(spp.occ$mean[spp.occ$year<=1976]))
  
  post.76.mean<-mean(spp.occ$mean[spp.occ$year>1976])
  post.76.se<-sd(spp.occ$mean[spp.occ$year>1976])/sqrt(length(spp.occ$mean[spp.occ$year>1976]))
  
  results.temp<-data.frame(i,pre.76.mean,pre.76.se,post.76.mean,post.76.se,slope.year)
  results<-rbind(results,results.temp)
} # end i in species loop