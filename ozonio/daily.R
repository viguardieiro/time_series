library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)
library(readr)
library(TTR)
library(randtests)

trainDaily <- read.csv("trainDaily.csv")

trainDaily$Date.Local <- as.Date(trainDaily$Date.Local)
startDate <- trainDaily$Date.Local[1]
trainDailyZoo <- zoo(x = trainDaily$O3.Mean, order.by = trainDaily$Date.Local)
trainDaily.ts <- ts(data = trainDaily$O3.Mean, start = c(2001, as.numeric(format(startDate, "%j"))),
                    frequency = 365)

plot(decompose(trainDaily.ts))

par(mfrow = c(1, 2))
acf(trainDaily.ts[1:730], main = "ACF on subset of train data")
pacf(trainDaily.ts[1:730], main = "PACF on subset of train data")

#test if there is tendency in a 2 year window
tend <- function(x){
  p <- runs.test(x)
  p$p.value
}

tend.w <- rollapply(trainDaily.ts, width = 730, FUN = tend, align = "left")
print(sum(tend.w > 0.005))
plot(tend.w)

nm <- function(x){
  naive_mod <- naive(head(x,n=length(x)-7), h = 7)
  fore <- data.frame(forecast(naive_mod))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.naive <- rollapply(trainDailyZoo, FUN = nm, width = 730, align = 'right')

print(mean(r.naive))

lm <- function(x){
  linear_mod <- lm(head(x,n=length(x)-7)~ind+month)
  fore <- data.frame(forecast(naive_mod))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.naive <- rollapply(trainDailyZoo, FUN = nm, width = 730, align = 'right')

print(mean(r.naive))

sesm <- function(x){
  se_model <- ses(head(x,n=length(x)-7), h = 7)
  fore <- data.frame(forecast(se_model))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.ses <- rollapply(trainDailyZoo, FUN = sesm, width = 730, align = 'right')

print(mean(r.ses))

hm <- function(x){
  holt_model <- holt(head(x,n=length(x)-7), h = 7)
  fore <- data.frame(forecast(holt_model))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.holt <- rollapply(trainDailyZoo, FUN = hm, width = 730, align = 'right')

print(mean(r.holt))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(1,0,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

prinnt(mean(r.arma10))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(1,1,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

print(mean(r.arma10))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(2,0,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

print(mean(r.arma10))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(2,1,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

print(mean(r.arma10))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(5,1,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

print(mean(r.arma10))

ar1m <- function(x){
  ar1 <- arima(head(x,n=length(x)-7),order=c(6,0,0))
  pred <- predict(ar1, n.ahead=7)
  err <- mean(abs(pred$pred-tail(x,7)))
  return(err)
}

r.arma10 <- rollapply(trainDailyZoo, FUN = ar1m, width = 730, align = 'right')

print(mean(r.arma10))

am <- function(x){
  arima_model <- auto.arima(head(x,n=length(x)-7))
  fore <- data.frame(forecast(arima_model, h=7))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.arima <- rollapply(trainDailyZoo, FUN = am, width = 730, align = 'right')

print(mean(r.arima))

## test data

testDaily <- read.csv("testDaily.csv")
testDaily$Date.Local <- as.Date(testDaily$Date.Local)
testDaily$month <- as.factor(format(testDaily$Date.Local, format = "%m"))
testDailyZoo <- zoo(x = testDaily$O3.Mean, order.by = testDaily$Date.Local)

predictions.test <- data.frame()

hm <- function(x){
  holt_model <- holt(head(x,n=length(x)-7), h = 7)
  fore <- data.frame(forecast(holt_model))[, "Point.Forecast"]
  err <- mean(abs(fore-tail(x,7)))
  return(err)
}

r.holt <- rollapply(testDailyZoo, FUN = hm, width = 730, align = 'right')

print(mean(r.holt))

hm_f <- function(x){
  holt_model <- holt(head(x,n=length(x)-7), h = 7)
  fore <- data.frame(forecast(holt_model))[, "Point.Forecast"]
  return(fore)
}

pred.holt <- rollapply(testDailyZoo, FUN = hm_f, width = 730, align = 'right')


mins <- apply(pred.holt, 1, min)
maxs <- apply(pred.holt, 1, max)

plot(tail(testDailyZoo, 366),  xlab = ("Date"), ylab = "O3 mean", main = "Holt model of O3 daily mean")
polygon(c(tail(testDaily$Date.Local,366), rev(tail(testDaily$Date.Local,366))),
        c(mins, maxs), col = rgb(0, 0, 0.8, 0.3))
lines(tail(testDaily$Date.Local,366),rowMeans(pred.holt), col = 'red')
legend("topright", legend=c("Predictions", "Real values", "Pred interval"), col=c('red','black', 'blue'), lty = 1:1, cex=0.8)
