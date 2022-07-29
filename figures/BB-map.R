##=====================================================================##
##                                                                     ##
##                          Map of Bristol Bay                         ##
##                                                                     ##
##=====================================================================##
# rm(list=ls(all=T)) 
pkgs<-c("rgdal","raster","maps","mapdata","maptools","rgeos","pryr", "scales","marmap", "readxl","sp","RColorBrewer","sf")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))

##===========================================================## directory
if(exists("homeDir")) { } else { homeDir<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }
setwd(file.path(paste0(homeDir,"/maps")))

##=================================================## main map coordinates
west<--162.2;east<--154.8;south<-56.8;north<-60.2;cexf<-1.5
xlim<-c(west,east);ylim<-c(south,north)
center<-c(mean(xlim),mean(ylim)) 

##================================================================## data
## level=0: country, level=1: states, level=2: counties
usa<-getData('GADM',country="USA",level=1) ## states
ak<-usa[usa@data$NAME_1 %in% c("Alaska"),]
##---------------------------------------------------## change resolution
tol<-0.001 ## low (0.1) to high (0.0001) resolution 
# usa<-gSimplify(usa,tol=tol,topologyPreserve=F)
ak<-gSimplify(ak,tol=tol,topologyPreserve=F)
##-------------------------------------------------------## altitude data
alt.usa<-getData('alt',country='USA')
alt<-alt.usa[[2]] ## Alaska (element 1 is continental US)
##----------------------------------## function for clipping spatial data
gClip<-function(shp,bb) { gIntersection(shp,as(extent(bb), "SpatialPolygons"),byid=T) } 
##-----------------------------------------------------------## AWC lakes
AWC_lakes<-readOGR("/Users/janohlberger/Dropbox/My Mac (Jans-MacBook-Pro.local)/Documents/Work/UW/SalmonSizeChange/SalmonSize/maps/AWC_2020_Layers_Lakes","AWC_2020_Layers")
#attributes(AWC_lakes);head(AWC_lakes@data)
b<-bbox(AWC_lakes);b[1,1]<-west;b[1,2]<-east;b[2,1]<-south;b[2,2]<-north
AWC_lakes_sub<-gClip(AWC_lakes,b) ## clip to specified coordinate box
##----------------------------------------------------------## AWC rivers
AWC_rivers<-readOGR("/Users/janohlberger/Dropbox/My Mac (Jans-MacBook-Pro.local)/Documents/Work/UW/SalmonSizeChange/SalmonSize/maps/AWC_2020_Layers_Rivers","AWC_2020_Layers")
#attributes(AWC_rivers);head(AWC_rivers@data)
b<-bbox(AWC_rivers);b[1,1]<-west;b[1,2]<-east;b[2,1]<-south;b[2,2]<-north
AWC_rivers_sub<-gClip(AWC_rivers,b) ## clip to specified coordinate box
##---------------------------------------------------------## AWC regions
AWC_regions<-readOGR("/Users/janohlberger/Dropbox/My Mac (Jans-MacBook-Pro.local)/Documents/Work/UW/SalmonSizeChange/SalmonSize/maps/AWC_2020_Layers_Regions","AWC_2020_Layers")
b<-bbox(AWC_regions);b[1,1]<-west;b[1,2]<-east;b[2,1]<-south;b[2,2]<-north
AWC_regions_sub<-gClip(AWC_regions,b) ## clip to specified coordinate box

##========================================## commercial fishing districts 
FishingAreas<-readOGR("/Users/janohlberger/Dropbox/My Mac (Jans-MacBook-Pro.local)/Documents/Work/UW/SalmonSizeChange/SalmonSize/maps/Commercial_Fisheries_Bristol_Bay_Salmon_Statistical_Areas", "Commercial_Fisheries_Bristol_Bay_Salmon_Statistical_Areas")
##-------------------------------------------------------## select areas
area_names<-c("Nushagak","Naknek-Kvichak","Egegik","Ugashik") #,"Togiak")
FishingAreas<-FishingAreas[FishingAreas@data$MANAGEMENT %in% area_names,]

##========================================## commercial fishing districts 
FishingDistricts<-readOGR("/Users/janohlberger/Dropbox/My Mac (Jans-MacBook-Pro.local)/Documents/Work/UW/SalmonSizeChange/SalmonSize/maps/Commercial_Fisheries_Bristol_Bay_Salmon_Districts", "Commercial_Fisheries_Bristol_Bay_Salmon_Districts")

##=============================================================## main map
plot_color<-FALSE
pdf("BB-map.pdf",width=6,height=5.25)
par(mar=c(5,5,3,0),mgp=c(1.5,0.25,0),tcl=-0.3,cex.lab=0.8,cex.axis=0.6)
##--------------------------------------------------------------## colors
if(plot_color) {
colfunc<-colorRampPalette(brewer.pal(9,"YlOrBr")) ## altitude
col_water<-"deepskyblue3" ## rivers and lakes
col_fishing<-"firebrick" ## fishing districts
cols_text<-c("gray40","#855A27","#C7AC80","#8D9121","#629D9B","gray40","#178BC9","#536373","#AE2633")
} else {
colfunc<-colorRampPalette(c("white","grey50")) ## altitude
col_water<-"slategray2" ## rivers and lakes
col_fishing<-"dimgray" ## fishing districts
# cols_text<-c("gray40","black","black","black","black","gray40","black","black","black")
cols_text<-c("gray60","#855A27","#C7AC80","#8D9121","#629D9B","gray60","#178BC9","#536373","#AE2633") ## only river names in color?
}
##----------------------------------------------## base map with elevation
col_alt<-colfunc(100)
plot(alt,col=col_alt,alpha=1,legend=F,add=F,xlim=xlim,ylim=ylim,colNA=NA, ylab="",xlab="",lwd=1,axes=F)
at_long<-seq(round(west),round(east),2)
axis(side=1,at=at_long,labels=paste0(at_long,"°W"),line=0)
at_lat<-seq(round(south),round(north),1)
axis(side=2,at=at_lat,labels=paste0(at_lat,"°N"),line=0)
##--------------------------------------------------------## AWC regions
# lines(AWC_regions_sub,col="black",lwd=0.1)
##------------------------------------------------## AWC lakes and rivers
plot(AWC_lakes_sub,col=col_water,add=T,border=col_water,lwd=0.01)
lines(AWC_rivers_sub,col=col_water,lwd=0.2)
##---------------------------------------------## fishing districts/areas
plot(FishingAreas,col=col_fishing,add=T,border=col_fishing,lwd=0.01)
##-------------------------------------------------------------## borders
plot(ak,border="black",lwd=0.5,col=NA,add=T)
##---------------------------------------------------------## text rivers
rnames<-c("Togiak\nRiver","Igushik\nRiver","Wood\nRiver","Nushagak\nRiver","Kvichak\nRiver","Alagnak\nRiver","Naknek\nRiver","Egegik\nRiver","Ugashik\nRiver") ## "Togiak\nRiver"
rlong<-c(-160.2,-159.1,-158.75,-157.9,-156.8,-156.2,-156.6,-157.0,-157.2)
rlat <- c(59.2,58.9, 59.15, 59.0, 59.2, 59.0, 58.75, 58.1, 57.6)
text(rlong-0.004,rlat-0.004,rnames,adj=c(0.5,0.5),cex=0.3*cexf,font=2,col=1)
text(rlong,rlat,rnames,adj=c(0.5,0.5),cex=0.3*cexf,font=2,col=cols_text)
##-----------------------------------------------## text management areas
#areas_long<-c(-158.4,-157.8,-158.05,-158.1)
#areas_lat <- c(58.5,58.6,58.25,57.6)
#text(areas_long,areas_lat,area_names,adj=c(0.5,0.5),cex=0.2*cexf)
##----------------------------------------------------------## other text
text(-159,58,"Bristol\nBay",adj=c(0.5,0.5),cex=0.5*cexf,col="black")
##---------------------------------------------------------------## scale
map.scale(-156.2,57.1,ratio=F,relwidth=0.1,cex=0.5) ## (2)
##----------------------------------------------------## map inset/insert
x1<--200;x2<-300;y1<-0;y2<-280;par(usr=c(x1,x2,y1,y2))
map("worldHires",c("Canada","USA","Mexico"),fill=T,col="grey80",border="white",lwd=0.1,xlab="",ylab="",xlim=c(-220,-60),ylim=c(30,70),add=T)
points(center[1],center[2],pch=0,lwd=1,cex=1,col="black") 
rect(xleft=-170,ybottom=16,xright=-56,ytop=76,border="gray",col=NA,lwd=1)
##---------------------------------------------------------------## save
box()
dev.off() 

##=================================## Map of North Pacific with SST areas
pdf("SST-map.pdf",width=6,height=6)
par(oma=c(1,1,1,1),mgp=c(1.5,0.25,0),tcl=-0.3)
##----------------------------------------------------------## map extent
west<--191;east<--149;south<-44;north<-66 ## 54 | 34
xlim<-c(west,east);ylim<-c(south,north)
##------------------------------------------------------------## base map
map("worldHires",mar=c(1,1,1,1),fill=T,col="grey90",lwd=0.1,xlim=xlim,ylim=ylim,xlab="",ylab="") 
##--------------------------------------------------------------## scale
map.scale(east-10,south+2,ratio=F,relwidth=0.15,cex=0.6) 
##-------------------------------------------------------------## labels
at_long<-seq(-190,-150,10)
axis(side=1,at=at_long,labels=paste0(at_long,"°W"),line=0)
at_lat<-seq(45,65,5)
axis(side=2,at=at_lat,labels=paste0(at_lat,"°N"),line=0)
##-----------------------------------------------------## SST Bristol Bay
rect(xleft=-163.1,ybottom=56.2,xright=-157.5,ytop=60.0,col=NA,lwd=2,lty=2,border="dodgerblue2")
##------------------------------------------------------## SST Bering Sea
rect(xleft=-180,ybottom=54.3,xright=-165,ytop=60,col=NA,lwd=2,lty=2,border="chocolate2")
##---------------------------------------------------## SST Gulf of Alaska
rect(xleft=-172.5,ybottom=46.7,xright=-157.5,ytop=52.4,col=NA,lwd=2,lty=2,border="goldenrod1")
##------------------------------------------------## SST Aleutian Islands
rect(xleft=-180,ybottom=50.5,xright=-159.4,ytop=58.1,col=NA,lwd=2,lty=2,border="forestgreen")
##---------------------------------------------------------------## save
box()
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##