###random forest for chloride
#install.packages("forestFloor","randomForest", "tidyverse")

library(forestFloor)
library(randomForest)
library(tidyverse)


dat = read_csv("data/GLEON NLA data combined 2007 and 2012_first_visit.csv")

dat_rf<-data.frame(dat[,c("CHLORIDE_MGL" ,"WSA_ECO9" ,"PCT_DEVELOPED_BSN"  ,"Residence_Time","PCT_AGRIC_BSN", "LAT_DD","PCT_FOREST_BSN" ,"TP_UGL" ,"DEPTHMAX"   )])

#random forest model
rf_model<-randomForest(x = log(dat_rf[complete.cases(dat_rf),][,3:length(dat_rf)]+1), y=log(dat_rf[complete.cases(dat_rf),][,1]), keep.inbag = T, importance = T)

rf_model   
varImpPlot(rf_model)

#description of forest floor analyses https://arxiv.org/pdf/1605.09196.pdf
ff_model<-forestFloor(rf_model,X = log(dat_rf[complete.cases(dat_rf),][,3:length(dat_rf)]+1),y= dat_rf[complete.cases(dat_rf),][,1])

#plot(ff_model , orderByImportance=T, plot_seq=1, xlim=c(0,10), ylim=c(-100,500))

Col=fcol(ff_model,1,orderByImportance=T)

par(mar=c(10,10,10,10))
plot(ff_model,c(2,5,6), col=Col,orderByImportance=T, xlab="log(Watershed Development+1)", ylab="Log difference from mean chloride")

#plotting predicted values against observed
plot(rf_model$predicted,rf_model$y)
#plotting predicted against covariates
plot(rf_model$predicted~ff_model$X$LAT_DD)
plot(rf_model$predicted~ff_model$X$PCT_DEVELOPED_BSN)
     