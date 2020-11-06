library(tseries)
library(fpp2)
library(EnvStats)
library(zoo)
library(car)

data <- auscafe
plot(data, 
     main = "Monthly expenditure on eating out in Australia", 
     ylab = "Expenditure")

#The data doesn't have stable variance
lambda <- BoxCox.lambda(data)
data.stabilized <- BoxCox(data, lambda)
plot(data.stabilized, 
     main = 'Monthly expenditure on eating out in Australia after BoxCox', 
     ylab = 'Value')

#Removing sazonality
stl.fit <- stl(data.stabilized, s.window = 12)
plot(stl.fit)
data.trend <- data.stabilized - stl.fit$time.series[, 1]
plot(data.trend, main = "Monthly expenditure on eating out - \n Stabilized and without seasonality", 
     ylab = "Value")


#Testing if is stationary
adf.test(data.trend)
#Como p-valor > 0.05, não rejeitamos a hipótese de que os dados não são estacionários

#Testing diff 1
adf.test(diff(data.trend))
# Como p-valor < 0.05, temos que é estacionário

plot(diff(data.trend), 
     main = 'auscafe series (diff 1)', 
     ylab = 'Value')

#ACF and PACF of all data
par(mfrow = c(2, 1))
acf(diff(data.trend), main = "ACF auscafe series (diff 1)")
pacf(diff(data.trend), main = "PACF auscafe series (diff 1)")

#Looks like AR(1) because of the first two lags. Models being tested are AR(1), AR(2), ARMA(1,1) and ARMA(2, 1). 

# AR(1)
eval_AR1 <- function(data){
  m <- Arima(data, order = c(1, 1, 0))
  return(AIC(m))
}
AIC_AR1 <- rollapply(data.trend, width = 24, FUN = eval_AR1)

# AR(1) forecast in the last window
model.AR1 <- Arima(data.trend[403:426], order = c(1, 1, 0))
summary(model.AR1)

par(mfrow = c(1, 1))
plot(forecast(model.AR1, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(3.4, 3.8))
lines(InvBoxCox(model.AR1$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[403:426], lambda = lambda))

# AR(2)
eval_AR2 <- function(data){
  m <- Arima(data, order = c(2, 1, 0), lambda = lambda, method="ML")
  return(AIC(m))
}
AIC_AR2 <- rollapply(data.trend, width = 24, FUN = eval_AR2)

model.AR2 <- Arima(data.trend[403:426], order = c(2, 1, 0))
summary(model.AR2)

plot(forecast(model.AR2, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(3.4, 3.8))
lines(InvBoxCox(model.AR2$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[403:426], lambda = lambda))

# ARMA(1,1)
eval_ARMA11 <- function(data){
  m <- Arima(data, order = c(1, 1, 1), lambda = lambda, method="ML")
  return(AIC(m))
}
AIC_ARMA11 <- rollapply(data.trend, width = 24, FUN = eval_ARMA11)

model.ARMA11 <- Arima(data.trend[403:426], order = c(1, 1, 1))
summary(model.ARMA11)

plot(forecast(model.ARMA11, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(3.4, 3.8))
lines(InvBoxCox(model.ARMA11$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[403:426], lambda = lambda))

# ARMA(2,1)
eval_ARMA21 <- function(data){
  m <- Arima(data, order = c(2, 1, 1), lambda = lambda, method="ML")
  return(AIC(m))
}
AIC_ARMA21 <- rollapply(data.trend, width = 24, FUN = eval_ARMA21)

model.ARMA21 <- Arima(data.trend[403:426], order = c(2, 1, 1))
summary(model.ARMA21)

plot(forecast(model.ARMA21, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(3.4, 3.8))
lines(InvBoxCox(model.ARMA21$fitted, lambda = lambda), col = 'red')
lines(InvBoxCox(data.trend[403:426], lambda = lambda))

# Comparison between AIC of the three models in each of the windows
plot(AIC_AR1, ylim = c(-150, 150), ylab = "AIC", main = "Comparison between AICs of models")
lines(AIC_AR2, col = "blue")
lines(AIC_ARMA11, col = "green")
lines(AIC_ARMA21, col = "red")
legend("topleft", legend=c("AR(1)", "AR(2)", "ARMA(1, 1)", "ARMA(2, 1)"), col=c('black','blue', 'green','red'), lty = 1:1, cex=0.8)

# The best model is AR(1)

# Residual analysis
checkresiduals(model.AR1, lag = 24)

jarque.bera.test(model.AR1$residuals)

plot(forecast(model.AR1, h = 3, lambda = lambda, biasadj = TRUE), include = 0, 
     xlim = c(0, 28), ylim = c(3.2, 4.1))
lines(InvBoxCox(model.AR1$fitted, lambda = lambda), col = 'red')
lines(data[403:426])
lines(InvBoxCox(data.trend[403:426], lambda = lambda), col = "blue")
legend("topleft", legend=c("Real", "Real without seasonality", "Prediction without seasonality"), col=c('black','blue', 'red'), lty = 1:1, cex=0.8)
