#LAGOS random forest

#install.packages("forestFloor","randomForest", "tidyverse")

library(forestFloor)
library(randomForest)
library(tidyverse)


dat = read_csv("data/LAGOS_ChlorideCovariates.csv")

#converting feet to meters
dat[dat$DepthUnits %in% c("feet","ft"),"MeasureDepth"]<-dat[dat$DepthUnits %in% c("feet","ft"),"MeasureDepth"]*.3048

dat[which(dat$MeasureDepth==-99),"MeasureDepth"]<-NA

#one hot encoding the connectivity types
oneHotMdl=function(x) {
  conn = factor(x)
  model.matrix(~conn+0)
}

conn_hotencode<-oneHotMdl(dat$lakeconnection)

dat_rf<-data.frame(dat[,c(6,10:26)], conn_hotencode)

dat_rf$id <- 1:nrow(dat_rf)
set.seed(1)
train <- dat_rf %>% dplyr::sample_frac(.8)
test  <- dplyr::anti_join(dat_rf, train, by = 'id')

#max depth is deeper than measure depth
#there's negative values of chloride
#huge outliers as well
#probably need a year column because it probably won't do well in non-familiar years if the dataset is dominated by certain years
#latitude, longitude, date
#-99 for measure depth

#random forest model
#warning this could take a while -- ~10min
rf_model<-randomForest(x = cbind(log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(2,4:6,9:18)]+1),train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(19:22)]), y=log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,1]+1), keep.inbag = T, importance = T, mtry=length(log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(2,4:6,9:22)]+1)), sampsize = 10000, ntree=500, strata = as.factor(train[complete.cases(train[,c(1,2,4:6,9:22)]),"lagoslakeid"]))

rf_model  
varImpPlot(rf_model)

##test set r2
pred_test<-predict(rf_model, data.frame(y=log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,1]+1),cbind(log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(2,4:6,9:18)]+1),train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(19:22)])))
y=log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,1]+1)
r2_test<-1 - (sum((y-pred_test)^2)/sum((y-mean(y))^2))



ff_lagos<-forestFloor(rf_model, X =cbind(log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(2,4:6,9:18)]+1),train[complete.cases(train[,c(1,2,4:6,9:22)]),][,c(19:22)]), y= log(train[complete.cases(train[,c(1,2,4:6,9:22)]),][,1]+1))

Col=fcol(ff_lagos,2,orderByImportance=T, alpha=.3)

plot(ff_lagos, 1:6, orderByImportance = T, limitY = F,plot_GOF = F, col=Col)
