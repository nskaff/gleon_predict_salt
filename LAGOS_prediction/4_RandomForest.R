
# title: "WI_chloride_randomforest"
# author: "Hilary Dugan"
# date: "3/4/2019"

library(forestFloor)
library(randomForest)
library(tidyverse)
library(scales)
library(sf)
library(LAGOSNE)
library(caret)
library(lubridate)
# Load data
datin = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv")


## Tidy data
dat <- datin %>% dplyr::filter(Chloride < 10000 & Chloride >=0) %>%
  dplyr::mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  dplyr::filter(!(coastdist < 4)) %>%
  dplyr::filter(ActivityStartDate > as.Date('1990-01-01')) %>%
  dplyr::mutate(iws_forest = iws_nlcd2011_pct_41 + iws_nlcd2011_pct_42 +iws_nlcd2011_pct_43) %>% 
  dplyr::mutate(iws_develop = iws_nlcd2011_pct_24 + iws_nlcd2011_pct_23) %>% 
   dplyr::group_by(ActivityStartDate, lagoslakeid) %>%
   dplyr::summarise_if(is.numeric,list(~mean) )%>%
   dplyr::left_join(distinct(dplyr::select(datin,lagoslakeid,lakeconnection,gnis_name,state_zoneid,State))) 
  # group_by(State) %>%
  # filter(n() > 10) %>% ungroup()


#adding connectivity dummies
oneHot=function(x) {
  conn = factor(x)
  apply(data.frame(model.matrix(~conn+0)),2,FUN=factor)
}

dat_conn_dummy <- oneHot(dat$lakeconnection)

dat <- cbind(data.frame(dat),data.frame(dat_conn_dummy))

log01 <- function(x){log(x + 0.001)} # log of columns 
dat_rf <- dat %>%
  # filter_all(all_vars(!is.na(.))) %>%
  mutate_at(vars(Chloride,wlconnections_allwetlands_count:winterseverity),log01) %>%
  mutate(cov_strat=ifelse(Chloride>5.9, "high", ifelse(Chloride>5.4,"vhigh", "low")), month=month(ActivityStartDate)) %>%filter(!is.na(iws_nlcd2011_pct_22))

sapply(dat_rf, function(x) sum(is.na(x))) # See if there are NA values

## Random Forest model 

rf_cov <- dat_rf %>% dplyr::select(#nhd_lat, nhd_long,
  # month,
  iws_forest,
  iws_nlcd2011_pct_81,
  iws_nlcd2011_pct_82,
  iws_nlcd2011_pct_21,
  iws_nlcd2011_pct_22,
  iws_develop,
  iws_roaddensity_density_mperha,
  rdDist_Interstate,
  winterseverity,
  rdDist_Roads,
  connDR_LakeStream:connIsolated)

###all lagos data for later predictions
allLagos = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  dplyr::mutate(iws_forest = iws_nlcd2011_pct_41 + iws_nlcd2011_pct_42 +iws_nlcd2011_pct_43) %>% 
  dplyr::mutate(iws_develop = iws_nlcd2011_pct_24 + iws_nlcd2011_pct_23) %>% 
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') %>% 
  filter(!lagoslakeid %in% dat$lagoslakeid)

allLagos <- allLagos %>%
  # filter_all(all_vars(!is.na(.))) %>%
  mutate_at(vars(iws_nlcd2011_ha_0:iws_develop),log01) %>%
  filter(!is.na(iws_nlcd2011_pct_22))

#sapply(allLagos, function(x) sum(is.na(x))) # See if there are NA values

dat_conn_dummy<-oneHot(allLagos$lakeconnection)
allLagos<-cbind(allLagos,dat_conn_dummy)
allLagos.rf <- allLagos %>% dplyr::select(colnames(rf_cov))








#######Don't run unless you've got some time####
##grid search to select random forest hyperparameters
control <- trainControl(method="oob", number=10, search="random")
rf_random <- train(y=dat_rf$Chloride, x = rf_cov, method="rf",tuneLength=15, trControl=control)
#best r2 model has mtry=4, but I like maximizing mtry to better interpret interactions in forestFloor plots/control for variables that are used for splits earlier in the trees
#small grid search for the right sampsize. Lower sampsize decorrelates trees a bit, which is important when you maximize mtry but less so if mtry is 4
sampsize<-c(100,200,500,1000,length(dat_rf$Chloride))
samp_df<-data.frame()

for (i in 1:length(sampsize) ){
  rf_sampsize<-randomForest(y=dat_rf$Chloride, 
                            x = rf_cov, 
                            keep.inbag = T, 
                            importance = T, 
                            ntree=500,
                            sampsize = sampsize[i],
                            mtry=length(rf_cov))
                            #mtry=7)
  samp_df<-rbind(samp_df,
                 data.frame(sampsize=sampsize[i],
                            r2=rf_sampsize$rsq[length(rf_sampsize$rsq)],
                            mse=rf_sampsize$mse[length(rf_sampsize$mse)])
  )
}
samp_df
#sampsize around the max is pretty good, though it's not the worst idea to reduce this value to decorrelate each tree
######

####final rf model####
# rf_model<-randomForest(y=dat_rf$Chloride,
#                        x = rf_cov,
#                        keep.inbag = T,
#                        importance = T,
#                        ntree = 50,
#                        sampsize=10000,
#                        mtry=10)
#                        # mtry=length(rf_cov))
# 
# rf_model
# varImpPlot(rf_model)
# ff_model<-forestFloor(rf_model,X = rf_cov,y= rf_model$y )
# Col = fcol(ff_model, 1, orderByImportance=T)
# plot(ff_model, col=Col, orderByImportance=T)
#####


####stratified version of RF####
unique_lakes<-unique(dat_rf$lagoslakeid)
lake_prop<-.7
ntree<-50
highval_prop<-.5
vhighval_prop<-.4
lowval_prop<-.1

rf_model_lst_preds<-lapply(1:ntree,function(i){
  
train_lakes<-sample(unique_lakes, size =ceiling(length(unique_lakes)*lake_prop) )

#https://stackoverflow.com/questions/51671856/dplyr-sample-n-by-group-with-unique-size-argument-per-group
dat_rf_strat <- data.frame(dat_rf  %>%
                               mutate(id=row_number()) %>%
                              filter(lagoslakeid %in% train_lakes) )
                           #%>%
                             # mutate(frq=ifelse(cov_strat=="high",
                             #                   ceiling(highval_prop*n()),
                             #                   ifelse(cov_strat=="vhigh",
                             #                          ceiling(vhighval_prop*n()),
                             #                   ceiling((lowval_prop)*n())))) %>%
                             #   group_by(cov_strat) %>%
                             #    nest() %>%
                             # mutate(v = map(data, 
                             #                ~sample_n(data.frame(.), 
                             #                          unique(.$frq), replace=T))) %>%
                             # unnest(v))



rf_cov_strat<-dat_rf_strat %>% select(colnames(rf_cov))

rf_model_lst<-randomForest(y=dat_rf_strat$Chloride, 
                       x = rf_cov_strat, 
                       keep.inbag = T, 
                       importance = T, 
                       ntree = 1,
                       replace=F,
                       sampsize=length(dat_rf_strat$Chloride),
                       ytest=dat_rf$Chloride,
                       xtest=rf_cov,
                       mtry=10,
                       norm.votes=F,
                       keep.forest=TRUE )

#predictions for all lagos
lagos_pred<-predict(rf_model_lst, newdata=allLagos.rf)

#removing insample predictions
rf_model_lst$test$predicted[dat_rf_strat$id]<-NA


return(list(rf_model_lst$test$predicted, lagos_pred,rf_model_lst))
#return(rf_model_lst$test$predicted)

}
)

#calculating oob predictions for training data
oob_preds_combined<-data.frame(rf_model_lst_preds[[1]][[1]])
for(i in 2:ntree){ oob_preds_combined<-
  cbind(oob_preds_combined,rf_model_lst_preds[[i]][[1]])
}
oob_preds_combined<-rowMeans(oob_preds_combined, na.rm=T)

plot(oob_preds_combined~dat_rf$Chloride, main=cor(oob_preds_combined, dat_rf$Chloride) ^ 2)


#calculating predictions for all lagos
alllagos_preds_combined<-data.frame(rf_model_lst_preds[[1]][[2]])
for(i in 2:ntree){alllagos_preds_combined<-
  data.frame(alllagos_preds_combined,rf_model_lst_preds[[i]][[2]])}
alllagos_preds_combined<-rowMeans(alllagos_preds_combined, na.rm=T)
#####



#you'll notice that your interpretation of the forestfloor plot will change as you adjust mtry and sampsize. 
# You can prioritize different things with these values

#determining # of trees to stabilize error 
plot(rf_model$mse~c(1:500))
plot(rf_model$rsq~c(1:500))
#pretty good by 100 trees




dat_rf = dat_rf %>% mutate(predicted = predicted)

library(lme4)
fitsO <- lm(predicted ~ Chloride, data=dat_rf) 
fitsO = data.frame(r2 = paste0('r2 = ',round(summary(fitsO)$r.squared,2)),
                   Chloride = 7,
                   predicted = 0.1)

fits <- lme4::lmList(predicted ~ Chloride | lakeconnection, data=dat_rf) 
fits1 = data.frame(r2 = paste0('r2 = ',round(summary(fits)$r.squared,2)), lakeconnection = unique(fits@groups), 
                   Chloride = 7,
                   predicted = 0.1)

p1 = ggplot(dat_rf, aes(x = exp(Chloride), y = exp(predicted), color = exp(iws_forest))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = paste0('Modeled chloride (n =',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
p1
# ggsave(p1,filename = 'Figure_RF.png',width = 8,height = 6)

p2 = ggplot(dat_rf, aes(x = exp(Chloride), y = exp(predicted), color = exp(iws_forest))) + geom_point() + geom_abline(linetype = 'dashed') +
  scale_colour_viridis_c(name = "% Forest") + 
  facet_wrap(~lakeconnection) +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = paste0('Modeled chloride (n =',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fits1, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw()
p2
# ggsave(p2,filename = 'Figure_RF_byconnection.png',width = 8,height = 6)

#plotting residuals over space
library(tigris)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)
dat_rf$residuals <- dat_rf$predicted-dat_rf$Chloride


# LAGOS region
ggplot(data=dat_rf) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=nhd_long, y=nhd_lat, col=abs(residuals), size = abs(residuals)), alpha=.5 )+
  scale_color_viridis_c(option="magma")+
  theme_bw()
# ggsave(filename = 'Figure_RF_modelResiduals.png',width = 8,height = 6)


# plotting the location of the weird points in the nhd_lat forest floor plot
ggplot()+
  geom_sf(data=states_sf[states_sf$NAME %in% c("Wisconsin", "Minnesota"),], fill="white")+
  geom_point(data=dat_rf[ff_model$FCmatrix[,"nhd_lat"]< (-.7),],aes(x=nhd_long, y=nhd_lat), size=2, alpha=.5 )+
  theme_bw()








############# ############# ############# ############# ############# ############# 
## plotsS ####

# Plot prediction histogram 
ggplot() + 
  geom_density(aes(x = exp(oob_preds_combined), fill = "r"), alpha = 0.3) +
  geom_density(data = dat_rf, aes(x = exp(Chloride), fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted")) +
  scale_fill_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted"))  + 
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Chloride'~(mg~L^-1))) +
  ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.4, angle = 90) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.4, angle = 90)


#plotting prediction histogram of stratified predictions vs observed training data
ggplot() + 
  geom_density( aes(x = exp(alllagos_preds_combined), fill = "r"), alpha = 0.3) +
  geom_density( aes(x = exp(oob_preds_combined), fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted")) +
  scale_fill_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted"))  + 
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Chloride'~(mg~L^-1))) +
  ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.4, angle = 90) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.4, angle = 90)

# ggsave(filename = 'Figure_LAGOSpredictions.png',width = 8,height = 6)


# write_csv(allLagos,'data6_output_data_allLagosPredictions.csv')

a = allLagos %>% filter(clPred > log(100)) %>% 
  st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)
b = allLagos %>% filter(clPred > log(50)) %>% 
  mutate(cols = 
           case_when(exp(clPred) < 100 ~ 1,
                     exp(clPred) >= 100 ~ 2,
                     exp(clPred) > 260 ~ 3)) %>% 
  mutate(expCl = exp(clPred)) %>% 
  st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)
plot(a['lake_area_ha'])

library(mapview)
library(viridisLite)
m = b %>% mapview(zcol = "expCl", col.regions = magma(10))
# mapview(a['clPred'], col.regions = sf.colors(10),add = T)

## create standalone .html
# mapshot(m, url = '~/Downloads/clPredmap.html')
