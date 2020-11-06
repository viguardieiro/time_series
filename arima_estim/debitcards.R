library(tseries)
library(fpp2)
library(car)
library(zoo)

data <- debitcards
plot(data, main = 'Retail Debit Card Usage In Iceland')

#The data doesn't have stable variance
lambda <- BoxCox.lambda(data)
data.stabilized <- BoxCox(data, lambda)
plot(data.stabilized, 
     main = 'Retail Debit Card Usage In Iceland after BoxCox', 
     ylab = 'Value')

#Removing sazonality
stl.fit <- stl(data.stabilized, s.window = 12)
plot(stl.fit)
data.trend <- data.stabilized - stl.fit$time.series[, 1]
plot(data.trend, main = "Debit Card Usage - \n Stabilized and without seasonality", 
     ylab = "Value")

#Testing if is starionary
adf.test(data.trend)
#P-value is not lower than 0.05

#Testing diff 1
adf.test(diff(data.trend))
#P-value is lower than 0.05

plot(diff(data.trend), 
     main = 'DebitCard Series (diff 1)', 
     ylab = 'Value')

#Looking at the ACF and PACF
par(mfrow = c(2, 1))
acf(diff(data.trend), main = "ACF DebitCard Series (diff 1)")
pacf(diff(data.trend), main = "PACF DebitCard Series (diff 1)")

#Looking 2 years window ACF and PACF
par(mfrow = c(2, 1))
acf(diff(data.trend)[1:24], main = "ACF DebitCard Series (diff 1)")
pacf(diff(data.trend)[1:24], main = "PACF DebitCard Series (diff 1)")

#Look likes AR(1), AR(2) or some ARMA(2, 1)
#AR1
eval_AR1 <- function(data){
  m <- Arima(data, order = c(1, 1, 0))
  return(AIC(m))
}
AIC_AR1 <- rollapply(data.trend, width = 24, FUN = eval_AR1)
par(mfrow = c(1, 1))
plot(AIC_AR1, main = "AR(1) models AIC for each window", ylab = "AIC")

model.AR1 <- Arima(data.trend[141:164], order = c(2, 1, 0))
summary(model.AR1)
par(mfrow = c(1, 1))
plot(forecast(model.AR1, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(20, 25))
lines(InvBoxCox(model.AR1$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[141:164], lambda = lambda))

#AR2
eval_AR2 <- function(data){
  m <- Arima(data, order = c(2, 1, 0), lambda = lambda)
  return(AIC(m))
}
AIC_AR2 <- rollapply(data.trend, width = 24, FUN = eval_AR2)
par(mfrow = c(1, 1))
plot(AIC_AR2, main = "AR(2) models AIC for each window", ylab = "AIC")

model.AR2 <- Arima(data.trend[141:164], order = c(2, 1, 0))
summary(model.AR2)
par(mfrow = c(1, 1))
plot(forecast(model.AR2, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(20, 25))
lines(InvBoxCox(model.AR2$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[141:164], lambda = lambda))

#ARMA(2, 1)
eval_ARMA21 <- function(data){
  m <- Arima(data, order = c(2, 1, 1), lambda = lambda)
  return(AIC(m))
}
AIC_ARMA21 <- rollapply(data.trend, width = 24, FUN = eval_ARMA21)
par(mfrow = c(1, 1))
plot(AIC_ARMA21, main = "ARMA(2, 1) models AIC for each window", ylab = "AIC")

model.ARMA21 <- Arima(data.trend[141:164], order = c(2, 1, 1))
summary(model.ARMA21)
par(mfrow = c(1, 1))
plot(forecast(model.ARMA21, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(20, 25))
lines(InvBoxCox(model.ARMA21$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[141:164], lambda = lambda))

#Comparison between AICs
plot(AIC_AR1, ylim = c(-150, -50), ylab = "AIC", main = "Comparison between AICs of models")
lines(AIC_AR2, col = "blue")
lines(AIC_ARMA21, col = "red")
legend("bottomleft", legend=c("AR(1)", "AR(2)", "ARMA(2, 1)"), col=c('black','blue', 'red'), lty = 1:1, cex=0.8)

#Chosen model is AR2
checkresiduals(model.AR2)

par(mfrow = c(1, 1))
plot(forecast(model.AR2, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(20, 25))
lines(InvBoxCox(model.AR2$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[141:164], lambda = lambda), col = "blue")
legend("topleft", legend=c("Real without seasonality", "Prediction without seasonality"), col=c('blue', 'red'), lty = 1:1, cex=0.8)

par(mfrow = c(1, 1))
arima_forecast_season <- sapply(as.data.frame(forecast(model.AR2, h =3)) + stl.fit$time.series[(165-12):(167-12), 1], 
                                InvBoxCox, 
                                lambda = lambda)
arima_fitted_season <- InvBoxCox(model.AR1$fitted + stl.fit$time.series[141:164, 1], lambda = lambda)

plot(data[141:164], type = 'l', ylab = "Value", xlab = "Date", xlim = c(1, 27), ylim = c(15, 28), main = "AR(2) Forecast on real scale")
lines(arima_fitted_season, col = 'red')
polygon(x = c(25, 26, 27, 27, 26, 25), y = c(arima_forecast_season[,4], arima_forecast_season[,5]), col = "#D5DBFF", border =  "NA")
polygon(x = c(25, 26, 27, 27, 26, 25), y = c(arima_forecast_season[,2], arima_forecast_season[,3]), col = "#596DD5", border = "NA")
points(x = c(25, 26, 27), y = arima_forecast_season[, 1], pch = 16, col = "#0000AA")
legend("bottomleft", legend=c("Real", "Prediction", "Point Forecast"), col=c('black', 'red', "#0000AA"), lty = 1:1, cex=0.8)

