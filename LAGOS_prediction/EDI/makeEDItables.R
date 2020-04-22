# title: "EDI datesets"
# author: "Hilary Dugan"
# date: "2019-12-03"

library(tidyverse)
library(sf)
library(LAGOSNE)
library(lubridate)


# Load data
datin = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv")
colNames = read_csv('LAGOS_prediction/ColNames.csv')
names(datin) = colNames$NewName
## Tidy data
# total data (80k, after 1990 69k, after taking mean of measurements from same day 35k, filter out deep measurements 35k)

dat <- datin %>% dplyr::filter(Chloride < 10000 & Chloride >=0) %>%
  dplyr::mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  dplyr::filter(ActivityDepthHeightMeasure.MeasureValue < 10 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  dplyr::filter(ActivityStartDate > as.Date('1990-01-01')) %>% 
  dplyr::group_by(ActivityStartDate, lagoslakeid) %>%
  dplyr::summarise_if(is.numeric,list(mean)) %>%
  dplyr::left_join(distinct(dplyr::select(datin,nhdid = NHD_LAKE_ID,lagoslakeid,lakeconnection,gnis_name,state_zoneid,state_name))) 
# dat = data.frame(dat)

# Not taking log of values for final dataset
dat_rf <- dat %>%
  # mutate_at(vars(logChloride = Chloride,LakeArea,WS.Area,wlconnections_allwetlands_count:RoadDistance, WinterSeverity),log01) %>%
  mutate(Month = month(ActivityStartDate)) %>%
  filter(!is.na(WS.Dev.Low)) %>% 
  filter(!is.na(TonsPerMile)) %>%
  mutate(id=row_number())

sapply(dat_rf, function(x) sum(is.na(x))) # See if there are NA values

################## Training lakes dataset ##################
modelData <- dat_rf %>% dplyr::select(lagoslakeid,nhdid,gnis_name,ActivityStartDate,Chloride:nhd_long,MaxDepth,state_name,
                                      Month,LakeArea,WS.Area,WinterSeverity,
                                   WS.OpenWater:WS.EmergentWetlands,WS.RoadDensity,
                                   InterstateDistance:RoadDistance)
sapply(modelData, function(x) sum(is.na(x))) # See if there are NA values

# Table description: Training dataset for QRF model, with 29,010 observed chloride concentrations 
write_csv(modelData,'LAGOS_prediction/EDI/lakeCL_trainingData.csv')

################## Prediction dataset ##################
# Table description: Lake chloride predictions for 49,432 lakes
lagos.out = read_csv('LAGOS_prediction/output_data_allLagosPredictions_2020_03_28.csv') %>% 
  select(-pred.mean, -PIrange, -obsLakes, -Month) %>% 
  mutate_at(vars(LakeArea,WS.Area,WS.OpenWater:RoadDistance, WinterSeverity),exp) %>% 
  mutate_at(vars(LakeArea,WS.Area,WS.OpenWater:RoadDistance, WinterSeverity),round, digits = 2)

sapply(lagos.out, function(x) sum(is.na(x))) # See if there are NA values

write_csv(lagos.out,'LAGOS_prediction/EDI/lakeCL_predictions.csv')

