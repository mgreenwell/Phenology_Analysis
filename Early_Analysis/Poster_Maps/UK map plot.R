

site.locs<-read.table("C:\\Users\\matgre\\Documents\\PhD\\BMS_Data_Analysis\\all_sites_with_east_north_1km_1969sites_updated2010list.txt",header=T)
head(site.locs)
names(site.locs)<-tolower(names(site.locs))
library(blighty)
library(SDMTools)
?blighty   # information 

blighty(place="set.England", parcol="chartreuse3", parbor="black", tlncol="blue4", tlnwdh=2)     # plot map of england
Scalebar(200, 0, distance=500,unit = "km", scale = 1, t.cex = 0.8)

site.list<-c(1111,117,1321,1312,1319,1311,1050,1322,29,1314,1309,1318,1323,1310,30,994)
site.locs.subset<-site.locs[site.locs$siteno%in%site.list,]

points(site.locs.subset$east/1000,site.locs.subset$north/1000,col="red",pch=16,cex=0.5,bg="red")
x<- c(530,451,472,462)      #uses OS map coordinates
y<- c(180,206,173,190)
names<-c("London", "Oxford", "Reading", "Wallingford")
points(x,y, pch=16,bg="black", col="black", cex=0.5)

rect(323, 100, 600, 250, density = NULL, col = NA, border = "black", lty = "solid", lwd = 2)

#close up map#
blighty(place="set.England", parcol="chartreuse3", parbor="black", sarcol="red", tlncol="blue4", tlnwdh=2, xlimits=c(400,600), ylimits=c(100, 250)) #plot south England
rect(323, 100, 600, 250, density = NULL, col = NA, border = "black", lty = "solid", lwd = 2)


###points(site.locs$east/1000,site.locs$north/1000)          # add all points points in km

site.list<-c(1111,117,1321,1312,1319,1311,1050,1322,29,1314,1309,1318,1323,1310,30,994)
site.locs.subset<-site.locs[site.locs$siteno%in%site.list,]

points(site.locs.subset$east/1000,site.locs.subset$north/1000,col=2,pch=19,cex=1,bg=2)

x<- c(451,462)      #uses OS map coordinates
y<- c(206,190)
names<-c("Oxford", "Wallingford")
points(x,y, pch=16,bg="black", col="black")
text(x+5, y-3, labels=names, adj=1.25)

x<- c(530)
y<- c(180)
names<-c("London")
points(x,y, pch=16, col="black", bg="black")
text(x, y+11, labels=names, adj=-0)
Scalebar(400,125, distance=100,unit = "km", scale = 1, t.cex = 0.8)

x<- c(472)
y<- c(173)
names<-c("Reading")
points(x,y, pch=16, col="black",bg="black")
text(x-2, y-10, labels=names, adj=-0)
Scalebar(400,125, distance=100,unit = "km", scale = 1, t.cex = 0.8)

