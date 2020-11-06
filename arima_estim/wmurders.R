library(tseries)
library(fpp2)
library(EnvStats)
library(zoo)
library(car)

data <- wmurders
par(mfrow = c(1, 1))
plot(data, 
     main = "Monthly female murder rate \n(per 100,000 standard population)", 
     ylab = "Rate")
#There is no need to transform variance

#Testing if is stationary
adf.test(data)
#Not stationary

#Diff 1
adf.test(diff(data))
#Low P-value, is stationary
par(mfrow = c(1, 1))
plot(diff(data), 
     ylab = 'Change', 
     main = "Monthly female murder rate change \n(per 100,000 standard population)", )

#ACF and PACF of all data
par(mfrow = c(2, 1))
acf(diff(data),
    main = "ACF Monthly female murder rate change")
pacf(diff(data),
     main = "PACF   Monthly female murder rate change")

#testing with diff 2
#Diff 2
adf.test(diff(data, differences = 2))
#Low P-value, is stationary
par(mfrow = c(1, 1))
plot(diff(data, differences = 2), 
     ylab = 'Change', 
     main = "Monthly female murder rate (diff 2) \n(per 100,000 standard population)", )

#ACF and PACF of all data
par(mfrow = c(2, 1))
acf(diff(data, differences = 2),
    main = "ACF Monthly female murder rate (diff 2)")
pacf(diff(data, differences = 2),
     main = "PACF   Monthly female murder rate (diff 2)")


#Going to use diff 1
#Looking at the ACF and PACF
#The window will be 20 years to predict 3
par(mfrow = c(2, 1))
acf(diff(data[1:20], differences =  2),
    main = "ACF Monthly female murder rate (diff 2) 20 years")
pacf(diff(data[1:20], differences = 2),
     main = "PACF Monthly female murder rate (diff 2) 20 years")

#Looks like AR(1)
model.AR1 <- Arima(data, order = c(1, 2,0 ), include.mean = FALSE)
summary(model.AR1)
par(mfrow = c(1, 1))
plot(forecast(model.AR1, h = 3))
lines(model.AR1$fitted, col = 'red')

#Residuals analysis
checkresiduals(model.AR1)
#The residuals have no correlation, and looks like a normal distribution
#qqplot
qqPlot(model.AR1$residuals, main = "AR(1) model residuals")

#jarque bera on residuals
jarque.bera.test(model.AR1$residuals)

#MA(1)
model.MA1 <- Arima(data, order = c(0, 2, 1), include.mean = FALSE)
summary(model.MA1)
par(mfrow = c(1, 1))
plot(forecast(model.MA1, h = 3))
lines(model.MA1$fitted, col = 'red')

#Residuals analysis
checkresiduals(model.MA1)
#The residuals have no correlation, and looks like a normal distribution
#qqplot
qqPlot(model.MA1$residuals, main = "MA(1) model residuals")

#jarque bera on residuals
jarque.bera.test(model.MA1$residuals)

#ARMA(1, 1)
model.ARMA11 <- Arima(data,  order = c(1, 2, 1), include.mean = FALSE)
summary(model.ARMA11)
par(mfrow = c(1, 1))
plot(forecast(model.ARMA11, h = 3))
lines(model.ARMA11$fitted, col = 'red')
#Residuals analysis
checkresiduals(model.ARMA11)
#The residuals have no correlation, and looks like a normal distribution
#qqplot
qqPlot(model.ARMA11$residuals, main = "ARMA(1, 1) model residuals")

#jarque bera on residuals
jarque.bera.test(model.ARMA11$residuals)

#The lowests AIC is ARMA(1, 1)
#final forecast
plot(forecast(model.ARMA11, h = 3))
lines(model.ARMA11$fitted, col = 'red')
legend("topleft", legend=c("Predictions", "Real values"), col=c('red','black'), lty = 1:1, cex=0.8)
