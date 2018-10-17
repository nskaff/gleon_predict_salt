library(mapview)
library(sp)
library(RColorBrewer)
library(ggplot2)

#loading in data
overlaps<-read.csv("data/gis for nick.csv", header=T)
chloride_2007_2012<-read.csv("data/NLA_chloride_2007_2012.csv", header=T)
pract_data<-read.csv("data/practice model 2007 data.csv", header=T)

#plotting overlaps
overlap_sp<-SpatialPoints(overlaps[,c("long", "lat")], proj4string = CRS("+init=epsg:4326"))

mapview(overlap_sp)

chloride_spatial<-SpatialPointsDataFrame(coords = chloride_2007_2012[,c("LON_DD83", "LAT_DD83")], data = chloride_2007_2012[,c(-35,-36)], proj4string = CRS("+init=epsg:4326") )

#plotting chloride
chloride_spatial@data$log_X2012_Chloride<-log(chloride_spatial@data$X2012_Chloride)
chloride_spatial@data$log_X2007_Chloride<-log(chloride_spatial@data$X2007_Chloride)

chloride_spatial@data$log_chloride_Difference<-chloride_spatial@data$chloride_Difference

#2012
pal <- colorRampPalette(rev(brewer.pal(9, "Spectral")))
chlr_data<-chloride_spatial[!is.na(chloride_spatial$X2012_Chloride),]
mapview(chlr_data, zcol="log_X2012_Chloride", col.regions=pal)


#2007
chlr_data<-chloride_spatial[!is.na(chloride_spatial$X2007_Chloride),]
mapview(chlr_data, zcol="log_X2007_Chloride", col.regions=pal)

#dif between 2007 2012
chlr_data<-chloride_spatial[!is.na(chloride_spatial$X2007_Chloride) & !is.na(chloride_spatial$X2007_Chloride) ,]

mapview(chlr_data[chlr_data@data$chloride_Difference<10 & chlr_data@data$chloride_Difference>-10,], zcol="chloride_Difference", col.regions=pal, map.types="OpenStreetMap")

#catchment size
pract_spatial<-SpatialPointsDataFrame(coords = pract_data[,c("LON_DD", "LAT_DD")], data = pract_data[,c(-24,-25)], proj4string = CRS("+init=epsg:4326") )

pract_spatial@data$log_Relative.Catchment.Size<-log(pract_spatial@data$Relative.Catchment.Size)

mapview(pract_spatial, zcol="log_Relative.Catchment.Size", col.regions=pal, legend=T, labels)


##histograms of chloride

#chlor 2012
ggplot() + geom_histogram(data=chloride_spatial@data, aes(x=X2012_Chloride), fill="red") + scale_x_continuous("Chloride 2012", trans="log10") 

#chlor 2007
ggplot() + geom_histogram(data=chloride_spatial@data, aes(x=X2007_Chloride), fill="blue") + scale_x_continuous("Chloride 2007", trans="log10") 

#diff in chlor
ggplot() + geom_histogram(data=chloride_spatial@data[chloride_spatial@data$chloride_Difference<100 & chloride_spatial@data$chloride_Difference>-100,], aes(x=chloride_Difference), fill="black") + scale_x_continuous("Chloride diff") 

#lake/watershed area hist
ggplot() + geom_histogram(data=pract_data, aes(x=Relative.Catchment.Size), fill="black") + scale_x_continuous("Relative Catchment Size", trans="log10") + theme(
  plot.margin = margin(12, 12, 12, 12))
