load('4.2RF_Environment.RData')
library(forestFloor)
library(ggplot2)
library(lattice)

rf_model_test<-ranger(dependent.variable.name='logChloride',
                 data=data.frame(logChloride=dat_rf$logChloride,rf_cov),
                 keep.inbag = TRUE, importance = "impurity", inbag=random_lake_samps,
                 num.trees=ntree)
ff_rf_model <- ranger_RFadaptor(rf_model,dat_rf$logChloride)

ffra = forestFloor(ff_rf_model,rf_cov,calc_np = T)

View(data.frame(ff=rowSums(ffra$FCmatrix)+mean(ff_rf_model$predicted),pred=ff_rf_model$predicted))

plot(ffra,plot_seq = 1:9, orderByImportance = T)

Col = fcol(ffra,1)

#med low dev
show3d(ffra, 1:2, 1:2, plot_GOF=T, col = Col)

#low dev crop
show3d(ffra, c(1,3), c(1,3), plot_GOF=T, col = Col)

ff_dat<-data.frame(ffra$X, ffra$FCmatrix)

sum_med_low_dev<-ffra$FCmatrix[,"WS.Dev.Med"]+ffra$FCmatrix[,"WS.Dev.Low"]

sum_low_dev_crop<-ffra$FCmatrix[,"WS.Dev.Med"]+ffra$FCmatrix[,"WS.Crops"]

ggplot()+ geom_point( aes(x=ffra$X$WS.Dev.Low, y=ffra$X$WS.Crops, color = sum_low_dev_crop)) + 
  scale_color_distiller(palette='Spectral') +
  theme_bw()



