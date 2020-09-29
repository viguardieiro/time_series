library(tseries)
library(randtests)

trainWeekly <- read.csv("trainWeekly.csv")
trainWeekly$Date.Local <- as.Date(trainWeekly$Date.Local)
startDate <- trainWeekly$Date.Local[1]
trainWeekly.ts <- ts(data = trainWeekly$O3.Mean, start = c(2001, format(startDate, "%U")), frequency = 52)

plot(decompose(trainWeekly.ts))
runs.test(trainWeekly.ts)
trainWeekly$month <- format(trainWeekly$Date.Local, format = "%m")
trainWeekly$ind <- seq(1, length(trainWeekly$Date.Local), 1)

## ----- sazonal dummies model
model.sz <- lm(trainWeekly$O3.Mean~trainWeekly$ind+trainWeekly$month)
summary(model.sz)
plot(model.sz$fitted.values, type = "l", col = 'red')
lines(trainWeekly$O3.Mean)