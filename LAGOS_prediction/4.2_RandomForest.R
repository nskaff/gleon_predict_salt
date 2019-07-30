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
library(cowplot)

# Load data
datin = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv")

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

# Function to take log of columsn 
log01 <- function(x){log(x + 0.001)} # log of columns 
dat_rf <- dat %>%
  mutate_at(vars(Chloride,lake_area_ha,iws_ha,wlconnections_allwetlands_count:rdDist_Roads),log01) %>%
  mutate(month = month(ActivityStartDate)) %>%
  filter(!is.na(iws_nlcd2011_pct_22)) %>% 
  filter(!is.na(TonsPerMile)) %>% 
  mutate(id=row_number())

sapply(dat_rf, function(x) sum(is.na(x))) # See if there are NA values

## Random Forest covariance matrix ####
# rf_cov <- dat_rf %>% dplyr::select(month,lake_area_ha,iws_ha,
#                                    iws_nlcd2011_pct_0:iws_roaddensity_density_mperha,
#                                    buffer500m_nlcd2011_pct_0:TonsPerMile)
## Random Forest covariance matrix ####
rf_cov <- dat_rf %>% dplyr::select(month,lake_area_ha,iws_ha,winterseverity,
                                   iws_nlcd2011_pct_11:iws_nlcd2011_pct_95,iws_roaddensity_density_mperha,
                                   coastdist,rdDist_Interstate:rdDist_Roads)
sapply(rf_cov, function(x) sum(is.na(x))) # See if there are NA values


# rf_cov <- dat_rf[,names(dat_rf) %in% tail(DF$w,10)]

# Old code to create case weights based on number of observations per lake 
# cw = dat_rf %>% dplyr::select(lagoslakeid) %>%
#   group_by(lagoslakeid) %>% mutate(m = n()) %>% 
#   ungroup() %>% 
#   mutate(cw = sqrt(max(m)/m))
# dat_rf$cw = round(cw$cw,0)
##ranger version of random forest
#generating a matrix of in-bag and out of bag observations
ntree = 10000

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

  # # Only 1s and 0s
  unique_lakes<-unique(dat_rf$lagoslakeid)
  lake_samp <- sample(unique_lakes, size =.95*length(unique_lakes), replace=F)
  samp = as.integer(dat_rf$lagoslakeid %in% lake_samp)

  ## Nick's original code
  # #take a full bootstrap sample of the in-sample lakes. Leaving this with replace F but can be adjusted later
  # expand_samp<-sample(as.numeric(row.names(dat_rf))[dat_rf$lagoslakeid %in% lake_samp ], replace=F )
  # 
  # #counting the number of bootstrap samples for each observation
  # samp_count<-plyr::count(expand_samp)
  # 
  # #joining the in-bag sample with the out of bag sample index
  # df<-full_join(samp_count, data.frame(x=as.numeric(row.names(dat_rf))[!(row.names(dat_rf) %in% samp_count$x)]), by="x")
  # 
  # #ordering by row number
  # samp<-as.numeric(df[order(as.numeric(df$x)),"freq"])
  # 
  # #converting NA to 0 for withheld lakes
  # samp[is.na(samp)]<-0
  return(samp)
}
)

## Run RF model ####
rf_model<-ranger(dependent.variable.name='Chloride',
                 data=data.frame(Chloride=dat_rf$Chloride,rf_cov),
                 inbag=random_lake_samps,
                 num.trees=ntree, quantreg = T,
                 importance = 'permutation',
                 keep.inbag = TRUE)
rf_model

## Save the model
# saveRDS(rf_model, "./LAGOS_prediction/RFmodel_2019_07_29.rds")
## load the model
# rf_model <- readRDS("./LAGOS_prediction/RFmodel_2019_07_25.rds")

quantiles = c(0.05,0.5,0.95)
oob_quantiles <- predict(rf_model, type = 'quantiles', quantiles=quantiles)

ggplot() + geom_point(aes(x = rf_model$predictions, y = oob_quantiles$predictions[,2])) +
  ylab('50th percentile RF model') + xlab('Model Prediction') + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw()

ggplot() + geom_point(aes(x = dat_rf$Chloride, y = oob_quantiles$predictions[,2]), color = 'grey50', alpha = 0.4) +
  geom_point(aes(x = dat_rf$Chloride, y = rf_model$predictions), color = 'red4', alpha = 0.4) +
  ylab('50th percentile RF model') + xlab('Observation Chloride') + 
  theme_bw()
summary(lm(oob_quantiles$predictions[,2] ~ dat_rf$Chloride))
summary(lm(rf_model$predictions ~ dat_rf$Chloride))

ggplot() + 
  geom_errorbar(aes(ymin=oob_quantiles$predictions[order(rf_model$predictions),1],
                    ymax=oob_quantiles$predictions[order(rf_model$predictions),3], 
                    x=1:length(rf_model$predictions)), alpha=.01) +
  geom_point(aes(y=oob_quantiles$predictions[order(rf_model$predictions),2], 
                 x=1:length(rf_model$predictions)), 
             color="gold", alpha = 0.5, size = 0.4, shape = 16) +
  geom_point(aes(y=rf_model$predictions[order(rf_model$predictions)], 
                  x=1:length(rf_model$predictions)), 
              color="red4") +
  xlab('Observation ID') + ylab('Predicted Chloride w/ PI') +
  theme_bw()
ggsave('LAGOS_prediction/Figure_PredictionsCI.png',width = 6,height = 4)


length(rf_model$predictions[rf_model$predictions < oob_quantiles$predictions[,1]])
length(rf_model$predictions[rf_model$predictions > oob_quantiles$predictions[,3]])
(1139+127)/length(dat_rf$Chloride)


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


#### Add predictions to dat df ####
dat.out = dat_rf %>% 
  dplyr::select(ActivityStartDate,lagoslakeid:maxdepth,lakeconnection,gnis_name:id) %>% 
  dplyr::mutate(pred = rf_model$predictions) %>% 
  mutate(pred.05 = oob_quantiles$predictions[,1]) %>% 
  mutate(pred.50 = oob_quantiles$predictions[,2]) %>% 
  mutate(pred.95 = oob_quantiles$predictions[,3]) %>% 
  mutate(PIspread = pred.95-pred.05) %>% 
  mutate(within = ifelse(Chloride >= pred.05 & Chloride <= pred.95, TRUE, FALSE))

# useID = pull(dat_rf %>% arrange(desc(CIspread)) %>% select(lagoslakeid) %>% slice(1:10))
table(dat.out$within)


# dat_rf$oob_preds_mean = oob_preds_mean
p1 = ggplot(dat.out, aes(x = Chloride, y = pred.50, color = log(maxdepth))) + 
  geom_point(alpha = 0.6, shape = 16, size = 0.8) +
  ylim(-3.1,8) + xlim(-3.1,8) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) + ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  geom_abline(linetype = 2) +
  labs(title = paste0('Median, Cor = ',round(cor(dat.out$pred.50, dat.out$Chloride, use = "complete.obs") ^ 2,2))) +
  scale_colour_viridis_c() +
  theme_bw() 
p2 =  ggplot(dat.out, aes(x = Chloride, y = pred, color = log(maxdepth))) + 
  geom_point(alpha = 0.6, shape = 16, size = 0.8) +
  ylim(-3.1,8) + xlim(-3.1,8) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) + ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  geom_abline(linetype = 2) +
  labs(title = paste0('Mean, Cor = ',round(cor(dat.out$pred, dat.out$Chloride, use = "complete.obs") ^ 2,2))) +
  scale_colour_viridis_c() +
  theme_bw() 

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 10, nrow = 2)
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
# Lagos Prediction
lagos_pred_Aug <- predict(rf_model, data = allLagos.rf)
quantiles = c(0.05, 0.5, 0.95)
lagos_pred_Aug_PI <- predict(rf_model, data = allLagos.rf,type="quantiles", quantiles=quantiles)

## New data prediction (https://github.com/imbs-hl/ranger/blob/master/R/predict.R)
terminal.nodes <- predict(rf_model, allLagos.rf, type = "terminalNodes")$predictions + 1
node.values <- 0 * terminal.nodes
for (tree in 1:ntree) {
  node.values[, tree] <- rf_model$random.node.values[terminal.nodes[, tree], tree]
}
node.values.mean = rowMeans(node.values)
node.values.quantiles = apply(node.values, 1, quantile, probs = c(0.05,0.5,0.95)) # These are the same as quantile function above

#### Hilary testing shit out ############################################
# t1 = predict(rf_model, data = allLagos.rf[3,], type='response')
# t1$predictions
# 
# t2 = predict(rf_model, data = allLagos.rf[3,],type="quantiles", quantiles=quantiles)
# t2$predictions
# 
# t3 = predict(rf_model, data = allLagos.rf[3,], predict.all = TRUE)
# quantile(t3$predictions,probs = c(0.05,0.5,0.95))
# mean(t3$predictions)
# 
# #OOB prediction
# node.values <- rf_model$random.node.values.oob
# ## New data prediction (https://github.com/imbs-hl/ranger/blob/master/R/predict.R)
# terminal.nodes <- predict(rf_model, allLagos.rf[3,], type = "terminalNodes")$predictions + 1
# node.values <- 0 * terminal.nodes
# for (tree in 1:ntree) {
#   node.values[, tree] <- rf_model$random.node.values[terminal.nodes[, tree], tree]
# }
# 
# quantile(node.values,probs = c(0.05,0.5,0.95))
# t(apply(node.values, 1, quantile, quantiles, na.rm=TRUE))
# mean(node.values)
####################################################################################
allLagos.out = allLagos %>% 
  mutate(predictionAug = lagos_pred_Aug$predictions) %>% 
  mutate(predictionAug2 = node.values.mean) %>% 
  mutate(prediction.05 = lagos_pred_Aug_PI$predictions[,1]) %>% 
  mutate(prediction.50 = lagos_pred_Aug_PI$predictions[,2]) %>% 
  mutate(prediction.95 = lagos_pred_Aug_PI$predictions[,3]) %>% 
  dplyr::select(lagoslakeid:maxdepth,predictionAug:prediction.95) %>% 
  mutate(PIrange = prediction.95-prediction.05) %>% 
  mutate(obsLakes = ifelse(lagoslakeid %in% dat_rf$lagoslakeid,TRUE,FALSE))
# write_csv(allLagos.out,'LAGOS_prediction/output_data_allLagosPredictions.csv')
allLagos.out = read_csv('LAGOS_prediction/output_data_allLagosPredictions.csv')

ggplot(data = allLagos.out) + 
  geom_errorbar(aes(ymin=prediction.05[order(predictionAug2)],
                    ymax=prediction.95[order(predictionAug2)], 
                    x=1:length(predictionAug2)), alpha=.01) +
  geom_point(aes(y=prediction.50[order(predictionAug2)], 
                 x=1:length(predictionAug2)), 
             color="gold", alpha = 0.5, size = 0.4, shape = 16) +
  geom_point(aes(y=predictionAug2[order(predictionAug2)], 
                 x=1:length(predictionAug2)), 
             color="red4") +
  xlab('Observation ID') + ylab('Predicted Chloride w/ PI') +
  theme_bw()
ggsave('LAGOS_prediction/Figure_Predictions_LAGOS_PI.png',width = 6,height = 4)

summary(filter(allLagos.out, obsLakes == TRUE)$PIrange)
summary(filter(allLagos.out, obsLakes == FALSE)$PIrange)
# write_csv(alllagos_preds_Aug,'output_data_allLagosPredictions.csv')

#### Create Mean Chloride DF ####
dat.out.mean = dat.out %>% dplyr::mutate(predicted = as.numeric(pred)) %>% 
  group_by(lagoslakeid) %>% 
  summarise(meanCl = mean(Chloride), min = min(Chloride), max = max(Chloride), medianCl = median(Chloride),
            pred = mean(predicted), pred.50 = mean(pred.50), pred.05 = min(pred.05), pred.95 = max(pred.95),
            lakeconn = first(lakeconnection), lat = first(nhd_lat), long = first(nhd_long), count = n()) %>%
  arrange(meanCl) %>% mutate(id = as.numeric(rownames(.))) %>% 
  mutate(withinPI = ifelse(min >= pred.05 & max <= pred.95, TRUE, FALSE)) %>% 
  mutate(residuals = pred-meanCl, residuals.50 = pred.50-meanCl) 

table(dat.out.mean$withinPI)
  
ggplot(dat.out.mean, aes(x = id, y = meanCl, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred), color = 'red3', alpha = 0.6, size = 0.8) +
  geom_linerange(aes(ymin = min, ymax = max), alpha = 0.6) +
  ylab('Range observed Chloride concentrations')


############# ############# ############# ############# ############# ############# 
## Prediction for LAGOS ####
# Compare LAGOS predictions to lakes in model 
dupLakes = allLagos.out %>% filter(lagoslakeid %in% dat.out.mean$lagoslakeid)
meanLakes_Lagos = dat.out.mean %>% left_join(dupLakes) %>% 
  mutate(diffDup = predictionAug2 - pred) %>% 
  arrange(lagoslakeid)

fits2 <- lm(predictionAug2 ~ meanCl, data=meanLakes_Lagos) 
fits2 = data.frame(r2 = paste0('r2 = ',round(summary(fits2)$r.squared,2)),
                   meanCl = 7,
                   predictionAug2 = 0.1)

ggplot(meanLakes_Lagos,aes(x = exp(meanCl),y = exp(predictionAug2))) + geom_point() +
  ylab(bquote('LAGOS Predicted August Chloride'~(mg~L^-1))) + xlab(bquote('Observed Mean Chloride'~(mg~L^-1))) +
  # labs(title = paste0('Modeled chloride (n = ',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_abline(linetype = 'dashed') +
  geom_text(data = fits2, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw()

lakes100 = allLagos.out %>% dplyr::filter(exp(predictionAug2) >= 100) %>% 
  dplyr::select(lagoslakeid:maxdepth, predictionAug)
lakes50 = allLagos.out %>% dplyr::filter(exp(predictionAug2) >= 50) %>% 
  dplyr::select(lagoslakeid:maxdepth, predictionAug)

# Plot prediction histogram ####
ggplot() + 
  geom_density(data = allLagos.out, aes(x = exp(prediction.05)), fill = "transparent", linetype = 'dashed', alpha = 0.3) +
  geom_density(data = allLagos.out, aes(x = exp(prediction.95)), fill = "transparent", linetype = 'dashed', alpha = 0.3) +
  geom_density(data = allLagos.out, aes(x = exp(predictionAug2), fill = "r"), alpha = 0.3) +
  geom_density(data = allLagos.out, aes(x = exp(prediction.50), fill = "n"), alpha = 0.3) +
  # geom_density(data = dat.out.mean, aes(x = exp(meanCl), fill = "b"), alpha = 0.3) +
  # scale_colour_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted")) +
  scale_fill_manual(name ="", values = c('n' = 'navy',"r" = "red", "b" = "blue"), labels=c("n" = "Median", "r" = "Mean"))  + 
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Chloride'~(mg~L^-1))) +
  ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.4, angle = 90) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.4, angle = 90)
ggsave('LAGOS_prediction/Figure_LAGOSpredictions.png',width = 7,height = 5)

# Plot prediction histogram ####
ggplot() + 
  geom_density(data = dupLakes, aes(x = exp(prediction.05)), fill = "transparent", linetype = 'dashed', alpha = 0.3) +
  geom_density(data = dupLakes, aes(x = exp(prediction.95)), fill = "transparent", linetype = 'dashed', alpha = 0.3) +
  geom_density(data = dupLakes, aes(x = exp(predictionAug2), fill = "n"), alpha = 0.3) +
  # geom_density(data = dat.out.mean, aes(x = exp(meanCl), fill = "b"), alpha = 0.3) +
  # scale_colour_manual(name ="", values = c("r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted")) +
  scale_fill_manual(name ="", values = c('n' = 'navy',"r" = "red", "b" = "blue"), labels=c("b" = "Observed", "r" = "Predicted"))  + 
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Chloride'~(mg~L^-1))) +
  ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.4, angle = 90) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.4, angle = 90)
############# ############# ############# ############# ############# ############# 

