# title: "WI_chloride_randomforest"
# author: "Hilary Dugan"
# date: "2020-03-27"

install.packages('~/Downloads/ranger_0.11.2.tar.gz', repos = NULL, type="source")

library(forestFloor)
library(randomForest)
library(ranger)
library(tidyverse)
library(scales)
library(sf)
library(LAGOSNE)
library(caret)
library(lubridate)
library(devtools)
library(corrplot)
library(cowplot)
library(patchwork)

source('fitMetrics.R')

# Plot data? #
plotdata = FALSE

## Load data ####
datin = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv")
colNames = read_csv('LAGOS_prediction/ColNames.csv')
names(datin) = colNames$NewName

## Tidy data ####
dat <- datin %>% dplyr::filter(Chloride < 10000 & Chloride >=0) %>%
  dplyr::mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  dplyr::filter(ActivityDepthHeightMeasure.MeasureValue < 10 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  dplyr::filter(ActivityStartDate > as.Date('1990-01-01')) %>% 
  dplyr::group_by(ActivityStartDate, lagoslakeid) %>%
  dplyr::summarise_if(is.numeric,list(mean) )%>%
  dplyr::left_join(distinct(dplyr::select(datin,lagoslakeid,NHD_LAKE_ID,lakeconnection,gnis_name,state_zoneid,state_name))) %>% 
  mutate(Month = month(ActivityStartDate)) %>%
  filter(!is.na(WS.Dev.Low)) %>% 
  filter(!is.na(TonsPerMile)) %>% 
  ungroup()

dat.select = dat %>% dplyr::select("lagoslakeid",nhdid = "NHD_LAKE_ID","gnis_name",
                                   "ActivityStartDate","Chloride","nhd_lat","nhd_long",
                                   "lakeconnection","MaxDepth","state_name","Month",
                                   "LakeArea","WS.Area","WinterSeverity","WS.OpenWater",
          "WS.Dev.Open","WS.Dev.Low","WS.Dev.Med","WS.Dev.High","WS.Barren","WS.DeciduousForest",
          "WS.EvergreenForest","WS.MixedForest","WS.Schrub","WS.Grassland","WS.PastureHay","WS.Crops",
          "WS.WoodyWetlands","WS.EmergentWetlands","WS.RoadDensity","InterstateDistance","RoadDistance")

# Function to take log of columsn 
log01 <- function(x){log(x + 0.001)} # log of columns 

# Take log of predictor variables 
dat_rf <- dat.select %>%
  mutate_at(vars(logChloride = Chloride,LakeArea:RoadDistance),log01)

sapply(dat_rf, function(x) sum(is.na(x))) # Check if there are NA values

## Random Forest covariance matrix, with only predictors selected ####
rf_cov <- dat_rf %>% dplyr::select(Month,LakeArea,WS.Area,WinterSeverity,
                                   WS.OpenWater:WS.EmergentWetlands,WS.RoadDensity,
                                   InterstateDistance:RoadDistance)

sapply(rf_cov, function(x) sum(is.na(x))) # Check if there are NA values

# Sampling routine to use 95% of lakes as in-bag samples  ####
ntree = 1000

# Custom inbag sampling routine 
random_lake_samps <- lapply(1:ntree, function(i){
  unique_lakes<-unique(dat_rf$lagoslakeid)
  lake_samp <- sample(unique_lakes, size =.95*length(unique_lakes), replace=F) # In-bag uses 95% of lakes
  samp = as.integer(dat_rf$lagoslakeid %in% lake_samp)
  return(samp)
}
)

## Run RF model ####
rf_model <- ranger(dependent.variable.name='logChloride',
                 data=data.frame(logChloride=dat_rf$logChloride,rf_cov),
                 inbag=random_lake_samps,
                 mtry = 4,
                 num.trees=ntree, quantreg = T,
                 importance = 'permutation',
                 keep.inbag = TRUE)
rf_model

## Save the model 
# saveRDS(rf_model, "./LAGOS_prediction/RFmodel_2020_03_27.rds")
## load the model
# rf_model <- readRDS("./LAGOS_prediction/RFmodel_2020_03_28.rds")

# Calculate oob quantiles
oob_quantiles <- predict(rf_model, type = 'quantiles', quantiles = c(0.05,0.50,0.95))

# compare mean to median predictions
ggplot() + geom_point(aes(x = dat_rf$logChloride, y = oob_quantiles$predictions[,2]), color = 'grey50', alpha = 0.4) +
  geom_point(aes(x = dat_rf$logChloride, y = rf_model$predictions), color = 'red4', alpha = 0.4) +
  ylab('50th percentile RF model') + xlab('Observation Chloride') + 
  theme_bw()

summary(lm(oob_quantiles$predictions[,2] ~ dat_rf$logChloride)) # r2 of all observations (log transformed)
rmsle(exp(oob_quantiles$predictions[,2]),exp(dat_rf$logChloride))

#variable importance
v<-as.numeric(rf_model$variable.importance)
w<-as.character(names(rf_model$variable.importance))
DF<-data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

ggplot(DF, aes(x=w, y=v,fill=v))+
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  ylab("Variable Importance") + xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F) +
  theme_bw()

#### Add predictions to dat df ####
dat.out = dat_rf %>% 
  dplyr::mutate(pred.mean = exp(rf_model$predictions)) %>% 
  mutate(pred.05 = exp(oob_quantiles$predictions[,1])) %>% 
  mutate(pred.50 = exp(oob_quantiles$predictions[,2])) %>% 
  mutate(pred.95 = exp(oob_quantiles$predictions[,3])) %>% 
  mutate(within = ifelse(Chloride >= pred.05 & Chloride <= pred.95, TRUE, FALSE))
# write_csv(dat.out,'LAGOS_prediction/output_data_datout.csv')
# dat.out = read_csv('LAGOS_prediction/output_data_datout.csv')
table(dat.out$within)

if (plotdata == T) {
  ggplot() + 
    geom_errorbar(aes(ymin=dat.out$pred.05[order(dat.out$pred.50)],
                      ymax=dat.out$pred.95[order(dat.out$pred.50)], 
                      x=1:length(dat.out$pred.50)), alpha=.01) +
    geom_point(aes(y=dat.out$pred.50[order(dat.out$pred.50)], 
                   x=1:length(dat.out$pred.50)), 
               color="gold", alpha = 0.5, size = 0.4, shape = 16) +
    xlab('Observation ID') + ylab('Predicted Chloride w/ PI') +
    scale_y_log10() +
    theme_bw()
  ggsave('LAGOS_prediction/Figures/Predictions_with_PI.png',width = 6,height = 4)
}

### Load LAGOS data ####
allLagos.raw = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') 
names(allLagos.raw) = colNames$NewLagos

allLagos <- allLagos.raw %>%
  dplyr::select(any_of(names(dat_rf))) %>%  #won't throw error for names that don't exist
  mutate(Month = 7) %>% 
  filter(!is.na(WS.Dev.Low)) %>% 
  mutate_at(vars(LakeArea,WS.Area,WS.OpenWater:WinterSeverity),log01)
  
sapply(allLagos, function(x) sum(is.na(x))) # Check if there are NA values

# Prediction data frame to use with QRF 
allLagos.rf <- allLagos %>% dplyr::select(all_of(colnames(rf_cov)))

# Lagos Prediction
lagos_pred_Aug <- predict(rf_model, data = allLagos.rf)
quantiles = c(0.05, 0.5, 0.95)
lagos_pred_Aug_PI <- predict(rf_model, data = allLagos.rf,type="quantiles", quantiles=quantiles)

## Alternative method for quantile prediction (https://github.com/imbs-hl/ranger/blob/master/R/predict.R)
terminal.nodes <- predict(rf_model, allLagos.rf, type = "terminalNodes")$predictions + 1
node.values <- 0 * terminal.nodes
for (tree in 1:ntree) {
  node.values[, tree] <- rf_model$random.node.values[terminal.nodes[, tree], tree]
}
node.values.mean = rowMeans(node.values) # same as lagos_pred_Aug$predictions
node.values.quantiles = apply(node.values, 1, quantile, probs = c(0.05,0.5,0.95)) # These are the same as quantile function above

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
allLagos.out = allLagos %>% 
  mutate(pred.mean = exp(lagos_pred_Aug$predictions)) %>% 
  mutate(pred.05 = exp(lagos_pred_Aug_PI$predictions[,1])) %>% 
  mutate(pred.50 = exp(lagos_pred_Aug_PI$predictions[,2])) %>% 
  mutate(pred.95 = exp(lagos_pred_Aug_PI$predictions[,3])) %>% 
  mutate(PIrange = pred.95 - pred.05) %>%
  mutate(PIrange_log = lagos_pred_Aug_PI$predictions[,3] - lagos_pred_Aug_PI$predictions[,1]) %>% 
  mutate(obsLakes = ifelse(lagoslakeid %in% dat_rf$lagoslakeid,TRUE,FALSE))
table(allLagos.out$obsLakes)

# write_csv(allLagos.out,'LAGOS_prediction/output_data_allLagosPredictions_2020_03_28.csv')
# allLagos.out = read_csv('LAGOS_prediction/output_data_allLagosPredictions.csv')
summary(filter(allLagos.out, obsLakes == TRUE)$PIrange)
summary(filter(allLagos.out, obsLakes == FALSE)$PIrange)

#### dat.out.mean Mean Chloride DF ####
dat.out.mean = dat.out %>% 
  group_by(lagoslakeid) %>% 
  summarise(meanCl = mean(Chloride), min = min(Chloride), max = max(Chloride), medianCl = median(Chloride),
            pred.mean = mean(pred.mean), pred.50 = median(pred.50),
            pred.05 = min(pred.05), pred.95 = max(pred.95),
            lakeconn = first(lakeconnection), lat = first(nhd_lat), long = first(nhd_long), count = n()) %>%
  arrange(meanCl) %>% mutate(id = as.numeric(rownames(.))) %>% 
  mutate(withinPI = ifelse(min >= pred.05 & max <= pred.95, TRUE, FALSE)) %>% 
  mutate(residuals.50 = abs(round(pred.50,2) - round(medianCl,2)))
table(dat.out.mean$withinPI)

# Fit metrics for median (in manuscript)
log.lm(dat.out$pred.50,dat.out$Chloride)
log.lm(dat.out.mean$pred.50,dat.out.mean$medianCl)
rmsle(dat.out$pred.50,dat.out$Chloride)
rmsle(dat.out.mean$pred.50,dat.out.mean$medianCl)

if (plotdata == T) {
ggplot(dat.out.mean, aes(x = id, y = medianCl, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred.50), color = 'red3', alpha = 0.6, size = 0.8) +
  geom_linerange(aes(ymin = min, ymax = max), alpha = 0.6) +
  scale_y_log10() +
  ylab('Range observed Chloride concentrations')
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  
## Prediction for LAGOS ####
# Compare LAGOS predictions to lakes in model 
dupLakes = allLagos.out %>% arrange(pred.50) %>%  
  filter(lagoslakeid %in% dat_rf$lagoslakeid)
undupLakes = allLagos.out %>% arrange(pred.50) %>%
  filter(!lagoslakeid %in% dat_rf$lagoslakeid)
  
test = dat.out %>% dplyr::filter(Month == 7) %>% left_join(dupLakes, by = c("lagoslakeid")) %>% 
  mutate(diff = abs(pred.50.x - pred.50.y)) %>% arrange(desc(diff))
ggplot(test,aes(x = Chloride, y = pred.50.x)) + geom_point() +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_abline(linetype = 'dashed') +
  theme_bw()
ggplot(test,aes(x = pred.50.x, y = pred.50.y)) + geom_point() +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_abline(linetype = 'dashed') +
  theme_bw()

# Predictions #### manuscript stats
lakes230 = allLagos.out %>% dplyr::filter(pred.50 >= 230) %>% 
  filter(!lagoslakeid %in% dat_rf$lagoslakeid)
lakes100 = allLagos.out %>% dplyr::filter(pred.50 >= 100) %>% 
  filter(!lagoslakeid %in% dat_rf$lagoslakeid)
lakes50 = allLagos.out %>% dplyr::filter(pred.50 >= 50) %>% 
  filter(!lagoslakeid %in% dat_rf$lagoslakeid)

1972 - 1727

lakes20 = allLagos.out %>% dplyr::filter(pred.50 >= 20, pred.50 <= 50) 
lakes0 = allLagos.out %>% dplyr::filter(pred.50 < 20) 

# percent of lakes tested 
2773/nrow(allLagos.out)

nrow(allLagos.out %>% dplyr::filter(pred.50 >= 50)) /nrow(allLagos.out)
nrow(lakes20)/nrow(allLagos.out) # manuscript stats
nrow(lakes0)/nrow(allLagos.out) # manuscript stats

nrow(allLagos.out) - nrow(lakes0) - nrow(lakes20)
# Model output 
summary(dat.out.mean$medianCl)
summary(dupLakes$pred.50) #training lakes prediction
summary(undupLakes$pred.50) #non training lakes prediction
summary(allLagos.out$pred.50) # manuscript stat 

# Save data environment 
#save.image(file='4.2RF_Environment_2020_03_28.RData')

               