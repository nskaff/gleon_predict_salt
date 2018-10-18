###random forest for chloride
library(forestFloor)
library(randomForest)
library(tidyverse)


dat = read_csv("data/GLEON NLA data combined 2007 and 2012_first_visit.csv")

dat_rf<-data.frame(dat[,c("CHLORIDE_MGL" ,"WSA_ECO9" ,"PCT_DEVELOPED_BSN"  ,"Residence_Time","PCT_AGRIC_BSN", "LAT_DD"   )])
rf_model<-randomForest(x = log(dat_rf[complete.cases(dat_rf),][,3:6]+1), y=log(dat_rf[complete.cases(dat_rf),][,1]), keep.inbag = T, importance = T)

rf_model   
varImpPlot(rf_model)

ff_model<-forestFloor(rf_model,X = log(dat_rf[complete.cases(dat_rf),][,3:6]+1),y= dat_rf[complete.cases(dat_rf),][,1])

#plot(ff_model , orderByImportance=T, plot_seq=1, xlim=c(0,10), ylim=c(-100,500))

Col=fcol(ff_model,2,orderByImportance=T)
plot(ff_model , col=Col,orderByImportance=T)

plot(rf_model$predicted,rf_model$y)
