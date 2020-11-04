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
adf.test(diff(data, 1))
#Low P-value, is stationary
par(mfrow = c(1, 1))
plot(diff(data, differences = 1), 
     ylab = 'Change', 
     main = "Monthly female murder rate change \n(per 100,000 standard population)", )

#ACF and PACF of all data
par(mfrow = c(2, 1))
acf(diff(data, differences = 1),
    main = "ACF Monthly female murder rate change")
pacf(diff(data, differences = 1),
     main = "PACF   Monthly female murder rate change")


#Going to use diff 1
#Looking at the ACF and PACF
#The window will be 20 years to predict 3
par(mfrow = c(2, 1))
acf(diff(data[1:20], 1),
    main = "ACF Monthly female murder rate change 20 years")
pacf(diff(data[1:20], 1),
     main = "PACF Monthly female murder rate change 20 years")

#Looks like normal(0, 1)
par(mfrow = c(2, 1))
hist(diff(data, 1), main = 'Hist. of monthly female murder rate change')
qqPlot(diff(data, 1), main = 'QQPlot')

#Test to verify
#Shapiro-Wilk test to verify normality
shapiro.test(diff(data, 1))
#Normal hypothesis is not rejected

#Jarque-Bera test to verify skewness
jarque.bera.test(diff(data, 1))
#Normal hypothesis is not rejected

#T test to verify mean igual 0
t.test(diff(data, 1))
#0 mean is not rejected

#Going to rollaply a 20 window to get tests

normallity.tests <- function(data){
  s <- shapiro.test(data)$p.value
  j <- jarque.bera.test(data)$p.value
  t <- t.test(data)$p.value
  return(c("Shapiro" = s, "Jarque" = j, "T" = t))
}

results <- rollapply(diff(data), width = 25, FUN = normallity.tests)
plot(results, main = "Normallity tests results")



#testing with diff 2
#Diff 2
adf.test(diff(data, 2))
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
acf(diff(data[1:20], 2),
    main = "ACF Monthly female murder rate (diff 2) 20 years")
pacf(diff(data[1:20], 2),
     main = "PACF Monthly female murder rate (diff 2) 20 years")

#Looks like AR(1)
model.AR1 <- Arima(diff(data, 2), order = c(1, 0,0 ), include.mean = FALSE)
model.AR1
par(mfrow = c(1, 1))
plot(forecast(model.AR1, h = 3))
lines(model.AR1$fitted, col = 'red')

#Residuals analysis
checkresiduals(model.AR1)
#The residuals have no correlation, and looks like a normal distribution
