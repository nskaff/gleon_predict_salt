###random forest for chloride
#install.packages("forestFloor","randomForest", "tidyverse")

library(forestFloor)
library(randomForest)
library(tidyverse)


dat = read_csv("data/GLEON NLA data combined 2007 and 2012_first_visit.csv")
#dat<-dat[which(dat$CHLORIDE_MGL<10000),]

dat_rf<-data.frame(dat[,c("CHLORIDE_MGL" ,"WSA_ECO9" ,"PCT_DEVELOPED_BSN"  ,"Residence_Time","PCT_AGRIC_BSN", "LAT_DD","PCT_FOREST_BSN" ,"TP_UGL" ,"DEPTHMAX"   )])

dat_rf$id <- 1:nrow(dat_rf)
train <- dat_rf %>% dplyr::sample_frac(.8)
test  <- dplyr::anti_join(dat_rf, train, by = 'id')



#random forest model
rf_model<-randomForest(x = log(train[complete.cases(train),][,3:length(train)]+1), y=log(train[complete.cases(train),][,1]), keep.inbag = T, importance = T)

rf_model  
varImpPlot(rf_model)

##test set r2
pred_test<-predict(rf_model, cbind(data.frame(y=log(test[complete.cases(test),][,1])),log(test[complete.cases(test),][,3:length(test)]+1), y=log(test[complete.cases(test),][,1])))
y=log(test[complete.cases(test),][,1])
r2_test<-1 - (sum((y-pred_test)^2)/sum((y-mean(y))^2))


#description of forest floor analyses https://arxiv.org/pdf/1605.09196.pdf
ff_model<-forestFloor(rf_model,X = log(train[complete.cases(train),][,3:length(train)]+1),y= train[complete.cases(train),][,1])

#plot(ff_model , orderByImportance=T, plot_seq=1, xlim=c(0,10), ylim=c(-100,500))

Col=fcol(ff_model,1,orderByImportance=T)

par(mar=c(10,10,10,10))
plot(ff_model,c(2,5,6), col=Col,orderByImportance=T, xlab="log(Watershed Development+1)", ylab="Log difference from mean chloride")

#plotting predicted values against observed
plot(rf_model$predicted,rf_model$y)
#plotting predicted against covariates
plot(rf_model$predicted~ff_model$X$LAT_DD)
plot(rf_model$predicted~ff_model$X$PCT_DEVELOPED_BSN)
     