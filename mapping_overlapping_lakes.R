library(mapview)
library(sp)
#loading in data
overlaps<-read.csv("data/gis for nick.csv")

overlap_sp<-SpatialPoints(overlaps[,c("long", "lat")], proj4string = CRS("+init=epsg:4326"))

mapview(overlap_sp)
