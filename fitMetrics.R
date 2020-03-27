rmse = function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

# Root Mean Squared Log Error
#LTRMSE #bennett 
rmsle <- function(actual, predicted, error = 0) {
  sqrt(mean((log(actual + error) - log(predicted + error)) ^ 2))
}

# Log transformed linear regression
#LTRMSE #bennett 
log.lm <- function(y, x, df, error = 0) {
  summary(lm(log(y) ~ log(x)))
}
