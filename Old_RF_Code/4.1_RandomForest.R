# title: "WI_chloride_randomforest"
# author: "Hilary Dugan"
# date: "4/5/2019"

library(forestFloor)
library(randomForest)
library(ranger)
library(tidyverse)
library(scales)
library(sf)
library(LAGOSNE)
library(caret)
library(lubridate)
library(parallel)
library(devtools)
library(corrplot)

# Load data
datin = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv")

df <- unclass(datin)

## Tidy data
# total data (80k, after 1990 69k, after taking mean of measurements from same day 35k, filter out deep measurements 35k)

dat <- datin %>% dplyr::filter(Chloride < 10000 & Chloride >=0) %>%
  dplyr::mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  # dplyr::filter(!(coastdist < 4)) %>%
  dplyr::filter(ActivityDepthHeightMeasure.MeasureValue < 10 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  # dplyr::filter(!is.na(maxdepth)) %>%
  dplyr::filter(ActivityStartDate > as.Date('1990-01-01')) %>%
  dplyr::group_by(ActivityStartDate, lagoslakeid) %>%
  dplyr::summarise_if(is.numeric,list(mean) )%>%
  dplyr::left_join(distinct(dplyr::select(datin,lagoslakeid,lakeconnection,gnis_name,state_zoneid,State))) 
dat = data.frame(dat)

#Correlation Matrix
png(file = "LAGOS_prediction/correlationPlot.png",width = 15,height = 15, units = 'in', res = 300)
  dat.cor = round(cor(dat[,8:71],use = 'complete.obs'),2)
  corrplot(dat.cor, method = "ellipse")
dev.off()


log01 <- function(x){log(x + 0.001)} # log of columns 
dat_rf <- dat %>%
  # filter_all(all_vars(!is.na(.))) %>%
  mutate_at(vars(Chloride,lake_area_ha,iws_ha,wlconnections_allwetlands_count:rdDist_Roads),log01) %>%
  mutate(month = month(ActivityStartDate)) %>%
  filter(!is.na(iws_nlcd2011_pct_22)) %>% 
  filter(!is.na(TonsPerMile)) %>% 
  mutate(id=row_number())
#Correlation Matrix on log values 
png(file = "LAGOS_prediction/correlationPlot_log.png",width = 15,height = 15, units = 'in', res = 300)
  dat.cor = round(cor(dat_rf[,8:71],use = 'complete.obs'),2)
  corrplot(dat.cor, method = "ellipse")
dev.off()

sapply(dat_rf, function(x) sum(is.na(x))) # See if there are NA values

## Random Forest model 


# rf_cov <- dat_rf[,names(dat_rf) %in% tail(DF$w,10)]

cw = dat_rf %>% dplyr::select(lagoslakeid) %>%
  group_by(lagoslakeid) %>% mutate(m = n()) %>% 
  ungroup() %>% 
  mutate(cw = sqrt(max(m)/m))
dat_rf$cw = round(cw$cw,0)


unique_lakes<-unique(dat_rf$lagoslakeid)
set.seed(20)
train_lakes<-sample(unique_lakes, .5*length(unique_lakes))

dat_rf_train<-dat_rf[dat_rf$lagoslakeid %in% train_lakes,]
dat_rf_test<-dat_rf[!(dat_rf$lagoslakeid %in% train_lakes),]

rf_cov_train <- dat_rf_train %>% dplyr::select(month,lake_area_ha,iws_ha,
                                   iws_nlcd2011_pct_0:iws_roaddensity_density_mperha,
                                   buffer500m_nlcd2011_pct_0:TonsPerMile)
sapply(rf_cov_train, function(x) sum(is.na(x))) # See if there are NA values

rf_cov_test <- dat_rf_test %>% dplyr::select(month,lake_area_ha,iws_ha,
                                               iws_nlcd2011_pct_0:iws_roaddensity_density_mperha,
                                               buffer500m_nlcd2011_pct_0:TonsPerMile)

##ranger version of random forest
#generating a matrix of in-bag and out of bag observations
ntree = 1000

random_lake_samps <- lapply(1:ntree, function(i){
  #print(i)
  
  # singleLake = dat_rf %>%
  #   group_by(lagoslakeid) %>% summarise(id = sample(id,size = 1)) %>%
  #   sample_frac(0.9)
  # samp = dat_rf %>% mutate(cw = case_when(
  #   id %in% singleLake$id & lagoslakeid %in% singleLake$lagoslakeid ~ 1,
  #   !id %in% singleLake$id & lagoslakeid %in% singleLake$lagoslakeid ~ NA_real_,
  #   !lagoslakeid %in% singleLake$lagoslakeid ~ 0)) %>% 
  #   pull(cw)
  
  # ## Hilary's condensed code (the same as Nick's?)
  # #Class weights
  # unique_lakes<-unique(dat_rf$lagoslakeid)
  # lake_samp = dat_rf$lagoslakeid %in% sample(unique_lakes, size =.95*length(unique_lakes), replace=F)
  # samp = dat_rf %>% mutate(use = ifelse(lake_samp == TRUE,cw,0)) %>%
  #   pull(use)
unique_lakes<-unique(dat_rf_train$lagoslakeid)
  # # Only 1s and 0s
  lake_samp <- sample(unique_lakes, size =.9*length(unique_lakes), replace=F)
  samp = as.integer(dat_rf_train$lagoslakeid %in% lake_samp)

  ## Nick's original code
  # #take a full bootstrap sample of the in-sample lakes. Leaving this with replace F but can be adjusted later
  # expand_samp<-sample(as.numeric(row.names(dat_rf_train))[dat_rf_train$lagoslakeid %in% lake_samp ], replace=F )
  # 
  # #counting the number of bootstrap samples for each observation
  # samp_count<-plyr::count(expand_samp)
  # 
  # #joining the in-bag sample with the out of bag sample index
  # df<-full_join(samp_count, data.frame(x=as.numeric(row.names(dat_rf_train))[!(row.names(dat_rf_train) %in% samp_count$x)]), by="x")
  # 
  # #ordering by row number
  # samp<-as.numeric(df[order(as.numeric(df$x)),"freq"])
  # 
  # #converting NA to 0 for withheld lakes
  # samp[is.na(samp)]<-0
  return(samp)
}
)

  
rf_model<-ranger(dependent.variable.name='Chloride',
                 data=data.frame(Chloride=dat_rf_train$Chloride,rf_cov_train),
                 inbag=random_lake_samps,
                 num.trees=ntree, quantreg = T
                 ,keep.inbag = TRUE 
                 )
rf_model


quantiles = c(0.05,0.95)
oob_quantiles<-predict(rf_model,  type = 'quantiles',quantiles=quantiles )

test_preds<-predict(rf_model, data=dat_rf_test)

test_preds_quant<-predict(rf_model, data=dat_rf_test, quantiles = c(.05, .5, .95),type = "quantiles")

r2_train<-cor(rf_model$predictions,dat_rf_train$Chloride )^2
plot(rf_model$predictions,dat_rf_train$Chloride)

r2_test<-cor(test_preds$predictions,dat_rf_test$Chloride )^2
plot(test_preds$predictions,dat_rf_test$Chloride )

r2_test_quant<-cor(test_preds_quant$predictions[,2],dat_rf_test$Chloride )^2
plot(test_preds_quant$predictions[,2],dat_rf_test$Chloride )

plot(test_preds_quant$predictions[,2], test_preds$predictions)


length(test_preds$predictions[test_preds$predictions<test_preds_quant$predictions[,1]])

length(test_preds$predictions[test_preds$predictions>test_preds_quant$predictions[,3]])





ggplot() + 
  geom_point( aes(y=test_preds$predictions[order(test_preds$predictions)], 
                  x=1:length(test_preds$predictions)), 
              color="red") +
  geom_errorbar(aes(ymin=test_preds_quant$predictions[order(test_preds$predictions),1],
                    ymax=test_preds_quant$predictions[order(test_preds$predictions),3], 
                    x=1:length(test_preds$predictions)), alpha=.01)


ggplot() + 
  geom_point( aes(y=rf_model$predictions[order(rf_model$predictions)], 
                  x=1:length(rf_model$predictions)), 
              color="red") +
  geom_errorbar(aes(ymin=oob_quantiles$predictions[order(rf_model$predictions),1],
                    ymax=oob_quantiles$predictions[order(rf_model$predictions),2], 
                    x=1:length(rf_model$predictions)), alpha=.01)


length(rf_model$predictions[rf_model$predictions<oob_quantiles$predictions[,1]])

length(rf_model$predictions[rf_model$predictions>oob_quantiles$predictions[,2]])


(1013+366)/length(dat_rf_train$Chloride)


# preds<-predict(rf_model, data=rf_cov,predict.all = T )
# 
# plot(testpredictions, dat_rf$Chloride)
# 
# testpredictions<-c()
# for (i in 1:nrow(test$predictions)){
# pred<-mean(preds$predictions[i,as.logical(sapply(random_lake_samps, function(x) x[i]))])
# 
# testpredictions[i]<-pred
# }
# 
# plot(rf_model$predictions, testpredictions)
# 
# plot(preds$predictions, rf_model$predictions)


#variable importance
v<-as.numeric(rf_model$variable.importance)
w<-as.character(names(rf_model$variable.importance))
DF<-data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

ggplot(DF, aes(x=w, y=v,fill=v))+
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  ylab("Variable Importance") + xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)
ggsave('LAGOS_prediction/Figure_ValueSummary.png',width = 6,height = 6)

##feature contributions for forestfloor
source("ranger_RFadaptor.R")

ff_rf_model<-ranger_RFadaptor(rf_model,dat_rf$Chloride)
ffra = forestFloor(ff_rf_model,rf_cov,calc_np = TRUE)

#color by most important feature
Col = fcol(ffra ,1)
plot(ffra, plot_seq=c(1,2,4,6,8),plot_GOF=F, limitY=F, col=Col,orderByImportance = T)

# ### Examine outliers ###
dat_rf$pred = rf_model$predictions
dat_rf$diff = dat_rf$pred - dat_rf$Chloride

# dat_rf$oob_preds_mean = oob_preds_mean
ggplot(dat_rf, aes(x = Chloride, y = pred, color = log(maxdepth))) + 
  geom_point(alpha = 0.6) +
  ylim(-3.1,8) + xlim(-3.1,8) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) + ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  geom_abline(linetype = 2) +
  labs(title = paste0('Cor = ',round(cor(dat_rf$pred, dat_rf$Chloride, use = "complete.obs") ^ 2,2))) +
  scale_colour_viridis_c() +
  theme_bw() 
ggsave('LAGOS_prediction/Figure_modelCor.png',width = 7,height = 5)



### Load LAGOS data ####
allLagos = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  mutate(month = 8) %>% 
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') 

allLagos <- allLagos %>%
  mutate_at(vars(lake_area_ha,iws_ha,iws_nlcd2011_ha_0:rdDist_Roads),log01) %>%
  filter(!is.na(iws_nlcd2011_pct_22))

sapply(allLagos, function(x) sum(is.na(x))) # See if there are NA values
allLagos.rf <- allLagos %>% dplyr::select(colnames(rf_cov))
lagos_pred_Aug <- predict(rf_model, data = allLagos.rf)
allLagos$predictionAug = lagos_pred_Aug$predictions

quantiles = c(0.05, 0.95)
lagos_pred_Aug_PI <- predict(rf_model, data = allLagos.rf,type="quantiles", quantiles=quantiles)

# write_csv(alllagos_preds_Aug,'output_data_allLagosPredictions.csv')

#### Create Mean Chloride DF ####
dat_rf.sum = dat_rf %>% dplyr::mutate(predicted = as.numeric(pred)) %>% 
  group_by(lagoslakeid) %>% 
  summarise(meanCl = mean(Chloride), min = min(Chloride), max = max(Chloride), medianCl = median(Chloride),
            pred = mean(predicted), lakeconn = first(lakeconnection), lat = first(nhd_lat), long = first(nhd_long), count = n()) %>%
  arrange(meanCl) %>% mutate(id = as.numeric(rownames(.))) %>% 
  mutate(residuals = pred-meanCl) 

ggplot(dat_rf.sum, aes(x = id, y = meanCl, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred), color = 'red3', alpha = 0.6, size = 0.8) +
  geom_linerange(aes(ymin = min, ymax = max), alpha = 0.6) +
  ylab('Range observed Chloride concentrations')

library(lme4)
fitsO <- lm(pred ~ meanCl, data=dat_rf.sum) 
summary(fitsO)
fitsO = data.frame(r2 = paste0('r2 = ',round(summary(fitsO)$r.squared,2)),
                   meanCl = 7,
                   pred = 0.1)

fits1 <- lme4::lmList(pred ~ meanCl | lakeconn, data=dat_rf.sum) 
fits1 = data.frame(r2 = paste0('r2 = ',round(summary(fits1)$r.squared,2)), lakeconn = unique(fits1@groups), 
                   meanCl = 7,
                   pred = 0.1)

ggplot(dat_rf.sum, aes(x = meanCl, y = pred, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred, color = log(count)),  alpha = 0.6) +
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.6) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  ylab(bquote('Mean Predicted Chloride'~(mg~L^-1))) +
  scale_color_viridis_c() +
  geom_abline(linetype = 'dashed') +
  theme_bw() +
  geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  coord_fixed(ratio = 1)
ggsave('LAGOS_prediction/Figure_modelCorMean_Range.png',width = 7,height = 5)


ggplot(dat_rf.sum) + geom_hline(yintercept = 0, linetype = 2) +
  geom_point(aes(x=count, y = residuals), color = 'red3', alpha = 0.6) +
  ylab(bquote('Log Mean Residual Chloride'~(mg~L^-1))) +
  xlab('Number of Observations') + 
  theme_bw()
ggsave('LAGOS_prediction/Figure_ModelResiduals.png',width = 7,height = 5)


p1 = ggplot(dat_rf.sum, aes(x = exp(meanCl), y = exp(pred))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Mean Chloride'~(mg~L^-1))) + xlab(bquote('Observed Mean Chloride'~(mg~L^-1))) +
  labs(title = paste0('Modeled chloride (n = ',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
p1
ggsave(plot = p1,'LAGOS_prediction/Figure_modelCorMean.png',width = 6,height = 5)


p2 = ggplot(dat_rf.sum, aes(x = exp(meanCl), y = exp(pred))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  facet_wrap(~lakeconn) +
  labs(title = paste0('Modeled chloride (n =',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fits1, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
p2
ggsave(plot = p2,'LAGOS_prediction/Figure_modelCorMean_LakeType.png',width = 6,height = 5)


############# ############# ############# ############# ############# ############# 
## Prediction for LAGOS ####

# Compare LAGOS predictions to lakes in model 
dupLakes = allLagos %>% filter(lagoslakeid %in% dat_rf.sum$lagoslakeid) %>% 
  dplyr::select(lagoslakeid, predictionAug)
meanLakes_Lagos = dat_rf.sum %>% left_join(dupLakes) %>% 
  mutate(diffDup = predictionAug - pred) %>% 
  arrange(lagoslakeid)

fits2 <- lm(predictionAug ~ meanCl, data=meanLakes_Lagos) 
fits2 = data.frame(r2 = paste0('r2 = ',round(summary(fits2)$r.squared,2)),
                   meanCl = 7,
                   predictionAug = 0.1)

ggplot(meanLakes_Lagos,aes(x = exp(meanCl),y = exp(predictionAug))) + geom_point() +
  ylab(bquote('LAGOS Predicted August Chloride'~(mg~L^-1))) + xlab(bquote('Observed Mean Chloride'~(mg~L^-1))) +
  # labs(title = paste0('Modeled chloride (n = ',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_abline(linetype = 'dashed') +
  geom_text(data = fits2, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw()

lakes100 = allLagos %>% dplyr::filter(exp(predictionAug) >= 100) %>% 
  dplyr::select(lagoslakeid:maxdepth, predictionAug)
lakes50 = allLagos %>% dplyr::filter(exp(predictionAug) >= 50) %>% 
  dplyr::select(lagoslakeid:maxdepth, predictionAug)
dplyr::rename(count(allLagos, state_zoneid,TonsPerMile), Freq = n)

# Plot prediction histogram ####
ggplot() + 
  geom_density(data = allLagos, aes(x = exp(predictionAug), fill = "r"), alpha = 0.3) +
  geom_density(data = dat_rf.sum, aes(x = exp(meanCl), fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted")) +
  scale_fill_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted"))  + 
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Chloride'~(mg~L^-1))) +
  ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.4, angle = 90) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.4, angle = 90)
ggsave('LAGOS_prediction/Figure_LAGOSpredictions.png',width = 7,height = 5)

############# ############# ############# ############# ############# ############# 
############# plotting residuals over space #############
library(tigris)
library(mapview)
library(viridisLite)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)

# LAGOS region
ggplot(data=dat_rf.sum) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=long, y=lat, col=abs(residuals), size = abs(residuals)), alpha=.5 )+
  scale_color_viridis_c(option="magma")+
  theme_bw()
# ggsave(filename = 'LAGOS_prediction/Figure_RF_modelResiduals.png',width = 7, height = 5)


b = allLagos %>% dplyr::select(lagoslakeid:lakeconnection,predictionAug) %>% 
  filter(predictionAug > log(50)) %>% 
  mutate(cols = 
           case_when(exp(predictionAug) < 100 ~ 1,
                     exp(predictionAug) >= 100 & exp(predictionAug) <260 ~ 2,
                     exp(predictionAug) > 260 ~ 3)) %>% 
  mutate(expCl = exp(predictionAug)) %>% 
  st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)

m = b %>% mapview(zcol = "expCl", layer.name = 'Predicted Chloride (mg/L)')
m
mapshot(m, url = paste0(getwd(), "/html/map.html"))
