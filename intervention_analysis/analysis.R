library(tseries)
library(zoo)
library(ggplot2)
library(forecast)
library(dplyr)
require(lmtest)
data <- read.csv('TOTBUSSMNSA.csv')
title = 'Total Business Sales FRED (FED St. Louis Economics Research)'
#data before intervention
pre_intervention <- data %>% filter(DATE < as.Date("2008-08-01"))
FRED_pre <- ts(pre_intervention$TOTBUSSMNSA,
           start = c(2002, 1),
           frequency = 12)

#all data
FRED <- ts(data$TOTBUSSMNSA, start = c(2002, 1), frequency = 12)

#check if variance is the same
autoplot(FRED, main = title) +
  scale_x_continuous(breaks = scales::extended_breaks(10))

#series need stabilization
lambda <- BoxCox.lambda(FRED)
lambda

#data after stabilization
autoplot(BoxCox(FRED, lambda), main = title) +
  scale_x_continuous(breaks = scales::extended_breaks(10))

autoplot(BoxCox(FRED_pre, lambda), main = title) + 
  scale_x_continuous(breaks = scales::extended_breaks(10))

#checking derivation

par(mfrow = c(2, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% plot(main = "Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% plot(main = "diff Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% diff() %>% plot(main = " diff diff Series")
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% diff(lag = 12) %>% plot(main = "diff diff(12) Series")


#checking the acfs
par(mfrow = c(2, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% acf(main = "diff Series", lag.max = 48)
FRED_pre %>% BoxCox(lambda = lambda) %>% diff(lag = 12) %>% acf(main = "diff(12) Series", lag.max =  48)

#checking the pacfs
#par(mfrow = c(1, 2))
FRED_pre %>% BoxCox(lambda = lambda) %>% diff() %>% pacf(main = "diff Series", lag.max = 48)
FRED_pre %>% BoxCox(lambda = lambda) %>%diff(lag = 12) %>% pacf(main = "diff(12) Series", lag.max =  48)


#seasonal ACF decays slowly
#non-season has lags 1 and 3 significant
#seasonal PACF have 3 significant lags
#non-seasonal PACF have lags 1 and 2 significant


model1 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(3, 0, 1), lambda = lambda)
print(summary(model1))

checkresiduals(model1)

model2 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(2, 0, 1), lambda = lambda)
print(summary(model2))

checkresiduals(model2)

model3 <- Arima(FRED_pre, order = c(2, 1,1), seasonal = c(2, 1, 1), lambda = lambda)
print(summary(model3))

checkresiduals(model3)

coeftest(model3)
