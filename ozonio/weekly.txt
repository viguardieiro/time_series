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

trainWeekly.ts_wt_seas <- trainWeekly.ts - decompose(trainWeekly.ts)$seasonal

plot(acf(trainWeekly.ts[1:104]))
plot(pacf(trainWeekly.ts[1:104]))

#------
#test if there is tendency in a 2 year window
tend <- function(x){
  t <- decompose(ts(x, frequency = 52, start = c(0, 0)))$trend
  x <- x - t
  p <- runs.test(x)
  p$p.value
}

tend.w <- rollapply(trainWeekly.ts_wt_seas, width = 104, FUN = tend, align = "left")
print(sum(tend.w > 0.005)/length(tend.w))
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

for(i in seq(1, 507)){
  if((i %% 4) == 1){
    jump = 1
  }else{
    jump = 0
  }
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
  
  
  #holt winters
  model.hw_add <- HoltWinters(o3.ts, beta = FALSE, seasonal = "additive")
  model.hw_mul <- HoltWinters(o3.ts, beta = FALSE, seasonal = "mult")
  
  #arma
  model.arma <- arima(o3.ts, order = c(1, 0 ,0))
  #model.arma <- auto.arima(o3.ts)
  
  newdata = data.frame(ind = (i+104):(i+107), 
                        month = trainWeekly$month[(i+104): (i+107)])
  
  #prediction
  pred.baseline <- rep(mean(o3[(length(o3)-4):length(o3)]), 4)
  pred.sazonal <- predict(model.sazonal, newdata = newdata)
  pred.linear <- predict(model.sazonal_linear, newdata = newdata)
  pred.poly_2 <- predict(model.sazonal_poly2, newdata = newdata)
  pred.poly_3 <- predict(model.sazonal_poly3, newdata = newdata)
  pred.hw_add <- coredata(forecast(model.hw_add, h = 4)$mean)
  pred.hw_mul <- coredata(forecast(model.hw_mul, h = 4)$mean)
  pred.arma <- coredata(forecast(model.arma, h = 4)$mean)
  
  #updating dataframe of predictions  
  temp <- data.frame(Date.Local = trainWeekly$Date.Local[(i+104): (i+107)],
                     pred.baseline = pred.baseline,
                     pred.sazonal = pred.sazonal,
                     pred.linear = pred.linear,
                     pred.poly_2 = pred.poly_2,
                     pred.poly_3 = pred.poly_3,
                     pred.hw_add = pred.hw_add,
                     pred.hw_mul = pred.hw_mul,
                     pred.arma = pred.arma,
                     real = trainWeekly$O3.Mean[(i+104): (i+107)],
                     ind = i,
                     jump = jump,
                     day = seq(1, 4))
  
  predictions <- rbind(predictions, temp)
}

#-------
#calculating residuals
residuals <- data.frame(baseline = predictions$pred.baseline - predictions$real,
                        sazonal = predictions$pred.sazonal - predictions$real,
                        linear = predictions$pred.linear - predictions$real,
                        poly_2 = predictions$pred.poly_2- predictions$real,
                        poly_3 = predictions$pred.poly_3- predictions$real,
                        hw_add = predictions$pred.hw_add- predictions$real,
                        hw_mul = predictions$pred.hw_mul - predictions$real,
                        arma_10 = predictions$pred.arma - predictions$real,
                        ind = predictions$ind,
                        jump = predictions$jump,
                        day = predictions$day)

write.csv(residuals, "auxRes.csv", row.names = FALSE)
predbest <- data.frame(sazonal = o3 - model.sazonal$fitted.values,
                       linear = o3 - model.sazonal_linear$fitted.values)
write.csv(predbest, "auxPred.csv", row.names = FALSE)
#total mae
#mean of each set of 4 weeks predictions
library(dplyr)
aux <- function(x){
  return(mean(abs(x)))
}
residuals.mae <- residuals %>% group_by(ind) %>% summarise_all(aux)
sort(sapply(residuals.mae, aux))

#residuals by day prediction
residuals.mae <- residuals %>% group_by(day) %>% summarise_all(aux)
residuals.mae

acf(o3 - model.sazonal$fitted.values, main = "ACF of Sazonal model residuals on training data ", xlim = c(0, 52), lag.max = 52)
qqPlot(o3 - model.sazonal$fitted.values, main = "QQPlot of sazonal model residuals", ylab = "Residual")

acf(o3 - model.sazonal_linear$fitted.values, main = "ACF of Linear model residuals on training data", xlim = c(0, 52), lag.max=  52)
qqPlot(residuals[residuals$jump == 1,]$linear, main = "QQPlot of linear model residuals", ylab = "Residual")

#------- prediction on test data

testWeekly <- read.csv("testWeekly.csv")
testWeekly$Date.Local <- as.Date(testWeekly$Date.Local)
testWeekly$month <- as.factor(format(testWeekly$Date.Local, format = "%m"))
predictions.test <- data.frame()
for(i in seq(1, 49)){
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
                     real = testWeekly$O3.Mean[(i+104): (i+107)],
                     ind = i,
                     day = seq(1, 4))
  predictions.test <- rbind(predictions.test, temp)
  
}
residuals.test <- data.frame(residuals = predictions.test$pred - predictions.test$real,
                             ind = predictions.test$ind, day = predictions.test$day)
temp <- residuals.test %>% group_by(ind) %>% summarise_all(aux)
aux(temp$residuals)

temp <- residuals.test %>% group_by(day) %>% summarise_all(aux)
temp
startDate <- as.Date("2015-05-03")
endDate <- as.Date("2016-04-24")

par(mfrow = c(2, 2))
p1 <- predictions.test %>% filter(day == 1)
plot(p1$Date.Local, p1$pred, type = 'l',  xlab = ("Date"), ylab = "O3 mean",
     xlim = c(startDate, endDate), ylim = c(0.005, 0.045), main = "Predictions for 1st week")
polygon(c(p1$Date.Local, rev(p1$Date.Local)),
        c(p1$low, rev(p1$up)), col = rgb(0, 0, 0.8, 0.3))
lines(p1$Date.Local, p1$real)
lines(p1$Date.Local, p1$pred, col = 'red')

p1 <- predictions.test %>% filter(day == 2)
plot(p1$Date.Local, p1$pred, type = 'l',  xlab = ("Date"), ylab = "O3 mean",
     xlim = c(startDate, endDate),  ylim = c(0.005, 0.045), main = "Predictions for 2nd week")
polygon(c(p1$Date.Local, rev(p1$Date.Local)),
        c(p1$low, rev(p1$up)), col = rgb(0, 0, 0.8, 0.3))
lines(p1$Date.Local, p1$real)
lines(p1$Date.Local, p1$pred, col = 'red')

p1 <- predictions.test %>% filter(day == 3)
plot(p1$Date.Local, p1$pred, type = 'l',  xlab = ("Date"), ylab = "O3 mean",
     xlim = c(startDate, endDate),  ylim = c(0.005, 0.045),main = "Predictions for 3rd week")
polygon(c(p1$Date.Local, rev(p1$Date.Local)),
        c(p1$low, rev(p1$up)), col = rgb(0, 0, 0.8, 0.3))
lines(p1$Date.Local, p1$real)
lines(p1$Date.Local, p1$pred, col = 'red')

p1 <- predictions.test %>% filter(day == 4)
plot(p1$Date.Local, p1$pred, type = 'l',  xlab = ("Date"), ylab = "O3 mean",
     xlim = c(startDate, endDate),  ylim = c(0.005, 0.045), main = "Predictions for 4th week")
polygon(c(p1$Date.Local, rev(p1$Date.Local)),
        c(p1$low, rev(p1$up)), col = rgb(0, 0, 0.8, 0.3))
lines(p1$Date.Local, p1$real)
lines(p1$Date.Local, p1$pred, col = 'red')

