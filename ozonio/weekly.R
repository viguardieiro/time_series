library(tseries)
library(zoo)
library(randtests)
library(forecast)
library(car)

trainWeekly <- read.csv("trainWeekly.csv")
trainWeekly$Date.Local <- as.Date(trainWeekly$Date.Local)
startDate <- trainWeekly$Date.Local[1]
trainWeekly.ts <- ts(data = trainWeekly$O3.Mean, start = c(2001, format(startDate, "%U")), frequency = 52)

plot(decompose(trainWeekly.ts))

plot(acf(trainWeekly.ts[1:104]))
plot(pacf(trainWeekly.ts[1:104]))

#------
#test if there is tendency in a 2 year window
tend <- function(x){
  p <- runs.test(x)
  p$p.value
}

tend.w <- rollapply(trainWeekly.ts, width = 104, FUN = tend, align = "left")
print(sum(tend.w > 0.005))
plot(tend.w)
#tendecy detected in every window

#------
#Model with sazonal dumies and tendency
# Z_t = f_t + T_t + a_t
# f_t is a seazonal dummie variable
# T_t is polynomial in t
#
#Holt model 
#Holt Winters model (addtive and multiplicative)

#variables for linear regression
trainWeekly$month <- as.factor(format(trainWeekly$Date.Local, format = "%m"))
trainWeekly$ind <- seq(1, length(trainWeekly$Date.Local), 1)

#data.frames por MAE and predictions

predictions <- data.frame()

for(i in seq(1, 611, 4)){
  #variables
  o3 <- trainWeekly$O3.Mean[i:(i+103)]
  startDate <- trainWeekly$Date.Local[i]
  o3.ts <- ts(o3, frequency = 52, start = c(format(startDate, "%Y"), format(startDate, "%U")))
  ind <- trainWeekly$ind[i:(i+103)]
  month <- trainWeekly$month[i:(i+103)]
  
  #models
  #linear regression
  model.sazonal <- lm(o3~month)
  model.sazonal_linear <- lm(o3~ind+month)
  model.sazonal_poly2 <- lm(o3~poly(ind, 2) + month)
  model.sazonal_poly3 <- lm(o3~poly(ind, 3) + month)
  
  #o3.test <- o3 - model.sazonal$fitted.values
  
  #param <- optim(c(0.1, 0.1), fn = optim_holt, gr = "L-BFGS-B")
  #holt method
  model.holt <- holt(o3.ts, h = 4)
  
  #holt winters
  model.hw_add <- HoltWinters(o3.ts, seasonal = "additive")
  model.hw_mul <- HoltWinters(o3.ts, seasonal = "mult")
  
  #arma
  model.arma <- arima(o3.ts, order = c(1, 0 ,0))
  #model.arma <- auto.arima(o3.ts)
  
  newdata = data.frame(ind = (i+104):(i+107), 
                        month = trainWeekly$month[(i+104): (i+107)])
  
  #prediction
  pred.naive <- rep(mean(o3[(length(o3)-4):length(o3)]), 4)
  pred.sazonal <- predict(model.sazonal, newdata = newdata)
  pred.linear <- predict(model.sazonal_linear, newdata = newdata)
  pred.poly_2 <- predict(model.sazonal_poly2, newdata = newdata)
  pred.poly_3 <- predict(model.sazonal_poly3, newdata = newdata)
  pred.holt <- coredata(model.holt$mean)
  pred.hw_add <- coredata(forecast(model.hw_add, h = 4)$mean)
  pred.hw_mul <- coredata(forecast(model.hw_mul, h = 4)$mean)
  pred.arma <- coredata(forecast(model.arma, h = 4)$mean)
  
  #updating dataframe of predictions  
  temp <- data.frame(Date.Local = trainWeekly$Date.Local[(i+104): (i+107)],
                     pred.naive = pred.naive,
                     pred.sazonal = pred.sazonal,
                     pred.linear = pred.linear,
                     pred.poly_2 = pred.poly_2,
                     pred.poly_3 = pred.poly_3,
                     pred.holt = pred.holt,
                     pred.hw_add = pred.hw_add,
                     pred.hw_mul = pred.hw_mul,
                     pred.arma = pred.arma,
                     real = trainWeekly$O3.Mean[(i+104): (i+107)])
  
  predictions <- rbind(predictions, temp)
}

#-------
#calculating residuals
residuals <- data.frame(naive = predictions$pred.naive - predictions$real,
                        sazonal = predictions$pred.sazonal - predictions$real,
                        linear = predictions$pred.linear - predictions$real,
                        poly_2 = predictions$pred.poly_2- predictions$real,
                        poly_3 = predictions$pred.poly_3- predictions$real,
                        holt = predictions$pred.holt- predictions$real,
                        hw_add = predictions$pred.hw_add- predictions$real,
                        hw_mul = predictions$pred.hw_mul - predictions$real,
                        arma_10 = predictions$pred.arma - predictions$real)
aux <- function(x){
  return(mean(abs(x)))
}
sort(sapply(residuals, aux))

plot(acf(residuals$sazonal), main = "Sazonal model  residuals ACF")
qqPlot(residuals$sazonal, main = "QQPlot of sazonal model residuals", ylab = "Residual")

plot(acf(residuals$linear), main = "Linear model  residuals ACF")
qqPlot(residuals$linear, main = "QQPlot of linear model residuals", ylab = "Residual")

plot(acf(residuals$poly_2), main = "Linear model  residuals ACF")
qqPlot(residuals$poly_2, main = "QQPlot of linear model residuals", ylab = "Residual")

#------- prediction on test data

testWeekly <- read.csv("testWeekly.csv")
testWeekly$Date.Local <- as.Date(testWeekly$Date.Local)
testWeekly$month <- as.factor(format(testWeekly$Date.Local, format = "%m"))
predictions.test <- data.frame()
for(i in seq(1, 52, 4)){
  #variables
  o3 <- testWeekly$O3.Mean[i:(i+103)]
  startDate <- testWeekly$Date.Local[i]
  o3.ts <- ts(o3, frequency = 52, start = c(format(startDate, "%Y"), format(startDate, "%U")))
  month <- testWeekly$month[i:(i+103)] 
  
  model.sazonal <- lm(o3~month)
  newdata = data.frame(month = testWeekly$month[(i+104): (i+107)])
  pred <- forecast(model.sazonal, newdata = newdata)
  temp <- data.frame(Date.Local = testWeekly$Date.Local[(i+104): (i+107)],
                     pred = pred$mean,
                     low = pred$lower[,2],
                     up = pred$upper[,2],
                     real = testWeekly$O3.Mean[(i+104): (i+107)])
  predictions.test <- rbind(predictions.test, temp)
  
}
residuals.test <- predictions.test$pred - predictions.test$real
print(aux(residuals.test))

startDate <- as.Date("2015-05-03")
endDate <- as.Date("2016-04-24")

plot(testWeekly$Date.Local, testWeekly$O3.Mean, type = 'l',  xlab = ("Date"), ylab = "O3 mean",
     xlim = c(startDate, endDate), main = "Sazonal model of O3 weekly mean")
polygon(c(predictions.test$Date.Local, rev(predictions.test$Date.Local)),
        c(predictions.test$low, rev(predictions.test$up)), col = rgb(0, 0, 0.8, 0.3))
lines(testWeekly$Date.Local, testWeekly$O3.Mean)
lines(predictions.test$Date.Local, predictions.test$pred, col = 'red')

#-------
#working if differenced series
trainWeekly$O3.diff <- c(NA,diff(trainWeekly$O3.Mean))
  trainWeekly.ts_diff <- diff(trainWeekly.ts)
plot(decompose(trainWeekly.ts_diff))

plot(acf(trainWeekly.ts_diff))
plot(pacf(trainWeekly.ts_diff))

tend_diff.w <- rollapply(trainWeekly.ts_diff, width = 104, FUN = tend, align = "left")
print(sum(tend_diff.w > 0.05))
plot(tend_diff.w)
#tendency not detected in many windows


#------
#data.frames por MAE and predictions
predictions.diff <- data.frame()

for(i in seq(2, 611, 4)){
  ii = (i -1)/4 + 1
  #variables
  o3 <- trainWeekly$O3.diff[i:(i+103)]
  startDate <- trainWeekly$Date.Local[i]
  o3.ts <- ts(o3, frequency = 52, start = c(format(startDate, "%Y"), format(startDate, "%U")))
  ind <- trainWeekly$ind[i:(i+103)]
  
  #models
  #linear regression
  model.linear <- lm(o3~ind)
  model.poly2 <- lm(o3~poly(ind, 2))
  model.poly3 <- lm(o3~poly(ind, 3))
  
  #ses
  model.ses <- ses(o3.ts, h = 4)
  
  #holt method
  model.holt <- holt(o3.ts, h = 4)
  
  #arma
  model.arma_11 <- arima(o3.ts, order = c(1,0, 1))
  model.arma_21 <- arima(o3.ts, order = c(2,1, 1))

  
  newdata = data.frame(ind = (i+104):(i+107))
  
  #prediction
  pred.naive <- rep(mean(o3[(length(o3)-4):length(o3)]), 4)
  pred.linear <- predict(model.linear, newdata = newdata)
  pred.poly_2 <- predict(model.poly2, newdata = newdata)
  pred.poly_3 <- predict(model.poly3, newdata = newdata)
  pred.ses <- coredata(model.ses$mean)
  pred.holt <- coredata(forecast(model.holt, h = 4)$mean)
  pred.arma_11 <- coredata(forecast(model.arma_11, h = 4)$mean)
  pred.arma_21 <- coredata(forecast(model.arma_21, h = 4)$mean)
  
  #updating dataframe of predictions  
  temp <- data.frame(Date.Local = trainWeekly$Date.Local[(i+104): (i+107)],
                     pred.naive = pred.naive,
                     pred.linear = pred.linear,
                     pred.poly_2 = pred.poly_2,
                     pred.poly_3 = pred.poly_3,
                     pred.ses = pred.ses,
                     pred.holt = pred.holt,
                     pred.arma_11 = pred.arma_11,
                     pred.arma_21 = pred.arma_21,
                     real = trainWeekly$O3.diff[(i+104): (i+107)])
  
  predictions.diff <- rbind(predictions.diff, temp)
}

residuals.diff <- data.frame(naive = predictions.diff$pred.naive - predictions.diff$real,
                        linear = predictions.diff$pred.linear - predictions.diff$real,
                        poly_2 = predictions.diff$pred.poly_2- predictions.diff$real,
                        poly_3 = predictions.diff$pred.poly_3- predictions.diff$real,
                        ses = predictions.diff$pred.ses - predictions.diff$real,
                        holt = predictions.diff$pred.holt- predictions.diff$real,
                        arma_11 = predictions.diff$pred.arma_11- predictions.diff$real,
                        arma_21 = predictions.diff$pred.arma_21 - predictions.diff$real)

sort(sapply(residuals.diff, aux))

plot(acf(residuals.diff$arma_21), main = "Linear model  residuals ACF")
qqPlot(residuals.diff$arma_21, main = "QQPlot of linear model residuals", ylab = "Residual")

plot(acf(residuals.diff$arma_11), main = "Linear model  residuals ACF")
qqPlot(residuals.diff$arma_11, main = "QQPlot of linear model residuals", ylab = "Residual")

