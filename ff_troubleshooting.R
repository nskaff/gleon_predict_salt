#load('4.2RF_Environment.RData')

rf_model_test<-ranger(dependent.variable.name='logChloride',
                 data=data.frame(logChloride=dat_rf$logChloride,rf_cov),
                 keep.inbag = TRUE, quantreg = T, importance = "impurity")
ff_rf_model <- ranger_RFadaptor(rf_model,dat_rf$logChloride)

ffra = forestFloor(ff_rf_model,rf_cov,calc_np = T)

View(data.frame(ff=rowSums(ffra$FCmatrix)+mean(rf_model_test$predicted),pred=rf_model_test$predicted))

plot(ffra,plot_seq = 1:9, orderByImportance = T)
