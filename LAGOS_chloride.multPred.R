
# WQP and Lagos data
# author: "Shannon LaDeau"
# date: "October 2018"
# Update: "Hilary Dugan - Dec 21 2018"

library(rjags)
library(tidyverse)
library(readr)
library(gridExtra)
library(dplyr)
library(forecast) # for box-cox test
library(beepr)

# Data used are from LAGOS and the WQP
datin = read_csv("data/LAGOS_ChlorideCovariates_wCoast.csv") 

#removing some insane outliers where cl is greater than 10,000 mg/l
dat <- datin %>% filter(Chloride < 10000 & Chloride >=0) %>%
  mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  mutate(edu_zoneid = ifelse(edu_zoneid == 'OUT_OF_EDU',1,edu_zoneid)) %>% #change regions to numbers
  mutate(region = parse_number(edu_zoneid))  %>%
  mutate(forag = (iws_nlcd2011_pct_ag+0.01)/(iws_nlcd2011_pct_forest+0.001)) %>%
  filter(Date > as.Date('2000-01-01')) %>%
  group_by(lagoslakeid) %>% 
  summarise_all(funs(first)) %>%
  filter(!(coastdist < 4 & Chloride > 1000))


# hist(log(dat$forag))
# sum(is.infinite(dat$forag))
table(dat$lakeconnection)
ggplot(dat, aes(x=lakeconnection, y=logChloride)) + 
  geom_boxplot()

##splitting data into training and testing set, with 80% of data in the training set
dat$id <- 1:nrow(dat)
train <- dat %>% dplyr::sample_frac(.8)
test  <- dplyr::anti_join(dat, train, by = 'id')

centerPred <- function(predictor, logadd = 0.01, bcval = 0.5){
  
  # library(MASS)
  # a = boxcox(maxdepth+0.001 ~ 1, data = dat)
  # mean(a$x[a$y > max(a$y)-qchisq(0.95,1)/2])

  # to find optimal lambda
  bc = BoxCox.lambda(predictor + logadd, method = 'loglik')
  # The optimal power transformation is found via the Box-Cox Test where
  # -1. is a reciprocal
  # -.5 is a recriprocal square root
  # 0.0 is a log transformation
  # .5 is a square root transform and
  # 1.0 is no transform.
  print(paste0('boxcox: ',bc))
  if (bc < bcval) {
    predictor = log(predictor + logadd)
  }
  
  cPred = predictor - mean(predictor,na.rm = T)
  hist(cPred)
  return(cPred)
}

y = train$Chloride  #focal response variable
region = train$region  #ecological region indicator
forag = log(train$forag)
depth = centerPred(train$maxdepth) #centered predictor
urban = centerPred(train$iws_nlcd2011_pct_impervious,logadd = 0.1)
ltype = c(1:4)[as.factor(train$lakeconnection)]
# ag = 0.01* centerPred(train$iws_nlcd2011_pct_ag,logadd = 0.01, bcval = 0)
# forest = 0.01* centerPred(train$iws_nlcd2011_pct_forest,logadd = 0.01, bcval = 0)
# lat = train$nhd_lat - mean(train$nhd_long,na.rm=T)  #centered predictor


####setting up testing parameters
y.test = test$Chloride  #focal response variable
region.test= test$region  #ecological region indicator
forag.test = log(test$forag)
depth.test = centerPred(test$maxdepth) #centered predictor
urban.test = centerPred(test$iws_nlcd2011_pct_impervious,logadd = 0.1)
ltype.test = c(1:4)[as.factor(test$lakeconnection)]

# If you're comfortable running the code then YOU SHOULD be able to scroll down to the section titled "add Multiple Predictor Variables" and skip the sequential models that add RE and single variable.
# Confirm convergence and define burn-in. ONCE you have a model you like you should do the steps below to make sure model is converged AND that you only use samples from posterior afer convergence.

##ignore this code for now
gelman.diag(jags.out)
GBR <- gelman.plot(jags.out)

# ##ignore this code for now
# ## determine the first iteration after convergence
# burnin <- GBR$last.iter[tail(which(GBR$shrink[,,2] > 1.1),1)+1]## check for no burn-in case
# if(length(burnin) == 0) burnin = 1
# ## remove burn-in
# jags.burn <- window(jags.out,start=burnin)
# ## check diagnostics post burn-in
# gelman.diag(jags.burn)


# Model with multiple predictor variables 
NormMean.Cov.RE <- "
model {
beta0 ~ dnorm(0,tau.beta0) # priors  
beta1~ dnorm(0,tau.beta1)
beta2~ dnorm(0,tau.beta2)
beta3~ dnorm(0,tau.beta3)

tau.obs ~ dgamma(a_obs,r_obs) #prior precision, data model

for(i in 1:N){
  mu[i] <- beta0 + beta1*depth[i]+ beta2*forag[i] + beta3*urban[i] +
       reg[region[i]] + typ[ltype[i]] # process model 
  y[i] ~ dnorm(mu[i],tau.obs) #data model
  urban[i]~dnorm(0.1,5)  ##prior based on existing summary
  forag[i]~dnorm(0,5)
  depth[i]~dnorm(0,5)
}
for(r in maxR) {  
  reg[r] ~ dnorm(0,tau_reg)
}
tau_reg  ~ dgamma(0.01,0.01) ##prior precision,  random effect

for(l in laketypes) {  
  typ[l] ~ dnorm(0,tau_typ)
}
tau_typ  ~ dgamma(0.01,0.01) ##prior precision,  random effect
}
"

# Define the data
data = list(N=length(y),y = log(y), region = region, ltype = ltype,
            maxR=sort(unique(dat$region)), 
            laketypes = sort(unique(ltype)),
            depth=depth, forag=forag, urban=urban,
            tau.beta0=1/0.01, tau.beta1=1/0.01, tau.beta2=1/0.01, tau.beta3=1/0.01,
            a_obs = 0.01, r_obs= 0.01 ) ## prior mean precision 

nchain = 3  # num chains for MCMC

##Set-up initial values - okay to use data to identify good starting points
inits <- list()
for(i in 1:nchain) {
  y.samp = sample(log(y),length(y),replace=TRUE)  #resamples orig data 
  inits[[i]] <- list(tau.obs=1/var((y.samp)), beta0=mean(y.samp), tau_reg=1.2, tau_typ = 1)
}

#### #### Run the model #### #### 
j.model <- jags.model(file = textConnection(NormMean.Cov.RE),
data = data,
inits = inits,
n.chains = 3)

jags.out <- coda.samples(model = j.model,
  variable.names = c("beta0","beta1", "beta2", "beta3","tau.obs","tau_reg","tau_typ"), 
  n.iter = 5000)

plot(jags.out)
out<-as.matrix(jags.out)
summary(out)

#DIC is easy way to look at model fit (generally look for decrease by 10 or more)
dic.samples(j.model,1000)  ##Deviance Information Criterion

# Getting predicted values and calculating training and testing R2 and rmse. This should be double checked..not totally sure of myself here
# plot prediction interval from mu and see if prediction y_pred is outside the prediction interval -- pull out the quantiles for the mu and look to see.
jags.out2 <- jags.samples(model = j.model,variable.names = c("beta0","beta1", "beta2", "beta3", "reg", "typ","mu", "y"),  n.iter = 5000)
beep(sound = 2)

summary(matrix(c(jags.out2[[1]],jags.out2[[2]],jags.out2[[3]],jags.out2[[4]])
               ,ncol=4))

#taking the mean of the posterior for each parameter estimated
posterior_means <- lapply(jags.out2, apply, 1, "mean")

###calculating training R2###
rss <- sum((posterior_means$mu- posterior_means$y) ^ 2, na.rm=T)  ## residual sum of squares
tss <- sum((posterior_means$y - mean(posterior_means$y, na.rm=T)) ^ 2, na.rm=T)  ## total sum of squares
rsq <- 1 - rss/tss

###calculating training rmse###
RMSE = function(pred, obs) {
  sqrt(mean((pred - obs)^2, na.rm=T))
}
###calculating training back transformed rmse###
RMSE_back = function(pred, obs){
  sqrt(mean((exp(pred) - exp(obs))^2, na.rm=T))
}
rmse <- RMSE(posterior_means$mu,posterior_means$y)
rmse_back <- RMSE_back(posterior_means$mu,posterior_means$y)

# out = data.frame(mu = posterior_means$mu,y = posterior_means$y)
# out$regions = train$region
# out$cols = viridis(91)[train$region]
# ggplot(data=out) + geom_point(aes(y=mu, x=y, color = regions)) + 
#   scale_color_gradientn(colours = viridis(n=91))

####calculating training back transformed median absolute error, which doesn't overweight huge errors on outliers like rmse does
#https://scikit-learn.org/stable/modules/model_evaluation.html#median-absolute-error
MAE_back <- function(pred , obs) {
  median(abs(exp(pred) - exp(obs)), na.rm=T)
}
mae_back <- MAE_back(posterior_means$mu,posterior_means$y)

#organizing the posterior parameter means
B <- as.matrix(unlist(posterior_means[c("beta0","beta1", "beta2", "beta3")]))

#organizing the random intercepts for each region
R.intercept<- as.matrix(unlist(posterior_means[c("reg")]))
#organizing the random intercepts for each region
R.intercept2<- as.matrix(unlist(posterior_means[c("typ")]))

###predicting from test set
y_hat_conditional<- B[1,] + B[2,]*depth.test+ B[3,]*forag.test + B[4,]*urban.test + 
          R.intercept[region.test] + R.intercept2[ltype.test]

#generating a credible interval on predicted values
#calculating predictive R
rss_cond <- sum((y_hat_conditional - log(y.test)) ^ 2, na.rm=T)  ## residual sum of squares
tss_cond <- sum((log(y.test) - mean(log(y.test), na.rm=T)) ^ 2, na.rm=T)  ## total sum of squares
rsq_conditional <- 1 - rss_cond/tss_cond

###calculating testing rmse###
rmse_test<-RMSE(y_hat_conditional,log(y.test))
###calculating testing rmse back and mae back transformed###
rmse_back_test<-RMSE_back(y_hat_conditional,log(y.test))
mae_back_test<-MAE_back(y_hat_conditional,log(y.test))


##creating a table with training and testing performance
fit.tbl<-data.frame(Fit=c( "RMSE","RMSE^e","MAE^e" ,"R^2"), Train=c(rmse,rmse_back,mae_back,rsq), Test=c(rmse_test,rmse_back_test,mae_back_test,rsq_conditional))
fit.tbl                 
#grid.table(fit.tbl)

########not sure how to calculate a marginal R square (not including the intercept) in a bayesian framework...help..
#another way of calculating predictive R2, essentially the same result
#cor(y_hat_conditional,log(y.test), use="pairwise.complete.obs")^2


############# 95% credible interval ##############################################
#Determining if predictions from data model fall within 95% credible interval from the process model on Shannon's suggestions. I'm having trouble remembering how to diagnose this, but I think we want the data model to fall within the credible interval most of the time
#taking the 95% credible intervals for the coefficient estimates in the training data
posterior_quantiles <- lapply(jags.out2, apply, 1,function(x){quantile(x,probs=c(.025,.975),na.rm = T)})

#determining what percent of the data model values (observed values) fall in the prediction interval for the training data
credible_int_train = data.frame(y_pred = posterior_means$y, cred_.025 = posterior_quantiles$mu[1,], 
                               cred_.975 = posterior_quantiles$mu[2,], inside = NA)
credible_int_train = credible_int_train %>% mutate(inside = ifelse(y_pred >= cred_.025 & y_pred <= cred_.975,1,0))
sum(credible_int_train$inside)/length(credible_int_train$inside) #proportion inside


#determining what percent of the data model values (observed values) fall in the prediction interval for the testing data

#generating predictions for each of the 5000 samples from the posterior of each model coefficient and calculating the predicted values for each iteration. rowMeans is taking the mean across each of the 3 chains (not sure why there's 3?)
y_hat_cred_intFUN <- function(row) {
  rowMeans(jags.out2$beta0[1,1:5000,]) +
  rowMeans(jags.out2$beta1[1,1:5000,])* depth.test[row]+
  rowMeans(jags.out2$beta2[1,1:5000,])*forag.test[row] +
  rowMeans(jags.out2$beta3[1,1:5000,])*urban.test[row] + 
  rowMeans(jags.out2$reg[1:91,1:5000,], dims=2,na.rm = T)[region.test[row]] +
  rowMeans(jags.out2$typ[1:4,1:5000,], dims=2,na.rm = T)[ltype.test[row]]
}  
y_hat_cred_intFUNout = sapply(X = seq(1:length(test$Chloride)), FUN = y_hat_cred_intFUN)
y_hat_cred_int = t(y_hat_cred_intFUNout)

#calculating the predicted credible interval for each observation in the testing set
posterior_quantiles_test <- apply(y_hat_cred_int, 1,function(x){quantile(x,probs=c(.025,.975), na.rm=T)})

#determining how many of the observed chloride values fall inside the credible interval for the predictions
credible_int_test = data.frame(y_pred = log(test$Chloride), cred_.025 = posterior_quantiles_test[1,], 
                               cred_.975 = posterior_quantiles_test[2,], inside = NA)

credible_int_test = credible_int_test %>% mutate(inside = ifelse(y_pred >= cred_.025 & y_pred <= cred_.975,1,0))
sum(credible_int_test$inside, na.rm=T)/length(credible_int_test$inside[!is.na(credible_int_test$inside)]) # proportion that falls in is very low

#plotting out predicted values and CIs
#training
credible_int_train$order <- order(credible_int_train$y_pred)
cred_train_plot_df<-cbind(credible_int_train[credible_int_train$order,],seq=1:nrow(credible_int_train))
ggplot(data=cred_train_plot_df) + geom_point(aes(y=y_pred, x=seq)) + geom_errorbar(aes(x=seq,ymin=cred_.025, ymax=cred_.975), color="red", size=.3)+ ggtitle("training predictions")+ scale_x_continuous("Observation #")


#testing
credible_int_test$order <- order(credible_int_test$y_pred)
cred_test_plot_df<-cbind(credible_int_test[credible_int_test$order,],seq=1:nrow(credible_int_test))
ggplot(data=cred_test_plot_df) + geom_point(aes(y=y_pred, x=seq)) + geom_errorbar(aes(x=seq,ymin=cred_.025, ymax=cred_.975), color="red", size=.3) + ggtitle("testing predictions") + scale_x_continuous("Observation #")



