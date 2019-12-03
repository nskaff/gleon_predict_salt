# A quantile regression forest (QRF) was used to model the relationship between observed chloride concentrations 
# and lake and watershed characteristics. This model was chosen to accommodate a large number of correlated predictor 
# variables, the presence of non-linear responses, and the potential importance of interactions among predictor variables. 
# The QRF was implemented with 1,000 trees using the ranger package in R (Wright and Ziegler 2017). 
# Wright, M. N., and A. Ziegler. 2017. ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software:1–17.

# author: Hilary Dugan and Nick Skaff
# date: 2019-12-03

library(tidyverse)
library(ranger)
library(scales)

# Load observation data
datin = read_csv("lakeCL_trainingData.csv")

# Function to take log of columns
log01 <- function(x){log(x + 0.001)} # log of columns 

# Take log of predictor variables 
dat_rf <- datin %>%
  mutate_at(vars(logChloride = Chloride,LakeArea:RoadDistance),log01)


## Random Forest covariance matrix, with only predictors selected ####
rf_cov <- dat_rf %>% dplyr::select(Month,LakeArea,WS.Area,WinterSeverity,
                                   WS.OpenWater:WS.EmergentWetlands,WS.RoadDensity,
                                   InterstateDistance:RoadDistance)

sapply(rf_cov, function(x) sum(is.na(x))) # See if there are NA values (Can't not have NA values in QRF)

ntree = 1000 # Set number of tress for QRF

random_lake_samps <- lapply(1:ntree, function(i){
  unique_lakes<-unique(dat_rf$lagoslakeid)
  lake_samp <- sample(unique_lakes, size =.95*length(unique_lakes), replace=F) # In-bag uses 95% of lakes
  samp = as.integer(dat_rf$lagoslakeid %in% lake_samp)
  return(samp)
}
)

## Run RF model ####
rf_model<-ranger(dependent.variable.name='logChloride',
                 data=data.frame(logChloride=dat_rf$logChloride,rf_cov),
                 inbag=random_lake_samps,
                 mtry = 4,
                 num.trees=ntree, quantreg = T,
                 importance = 'permutation',
                 keep.inbag = TRUE)
rf_model

# Get prediction interval 
quantiles = c(0.05,0.5,0.95)
oob_quantiles <- predict(rf_model, type = 'quantiles', quantiles=quantiles)


#### Add predictions to data frame (these are log values) ####
dat.out = dat_rf %>% 
  mutate(prediction.05 = oob_quantiles$predictions[,1]) %>% 
  mutate(prediction.50 = oob_quantiles$predictions[,2]) %>% 
  mutate(prediction.95 = oob_quantiles$predictions[,3])

# Plot observed chloride concentrations vs. predicted chloride concentations #
ggplot(dat.out, aes(x = Chloride, y = exp(prediction.50))) +
  geom_point(alpha = 0.8, shape = 21, size = 0.8, fill = 'gold3') +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) + ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  geom_abline(linetype = 'dashed') +
  scale_y_continuous(trans = log2_trans(),limits = c(0.1,3000)) + scale_x_continuous(trans = log2_trans(),limits = c(0.1,3000)) +
  annotate("text",x = 2.5, y = 600, size = 3,
           label = paste0('r2 = ',
                          round(cor(dat.out$prediction.50, dat.out$logChloride, use = "complete.obs") ^ 2,2))) +
  labs(title = paste0('Per observation (n = ',nrow(dat.out),')')) +
  theme_bw()

