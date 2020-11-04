---
title: "Modelling daily ozonio mean"
author: "Giovani Valdrighi, Vitória Guardieiro"
date: "30/09/2020"
output:
  beamer_presentation: default
  powerpoint_presentation: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Data

## Daily data

- New York data from 15/07/2001 to 30/04/2016.

```{r, message=FALSE}
library(ggplot2)
library(dplyr)
library(VIM)
library(zoo)
library(tseries)
library(randtests)
library(forecast)
library(car)

data <- read.csv("NY.csv")
data <- data %>% select(Date.Local, O3.Mean) %>% mutate(Date.Local = as.Date(Date.Local))
data <- data[-1, ]
startDate <- data$Date.Local[1] #First observed sunday
finalDate <- data$Date.Local[length(data$Date.Local)] #Last observed day
days <- seq(from = startDate, to = finalDate, by = 1)
dayWeek <- seq(from = startDate, to =  finalDate, by = 7)
#fulldata have NA rows in days that there is no observation
fulldata <- data.frame(Date.Local = days)
fulldata <- as.data.frame(merge(data, fulldata, all.y = TRUE)) %>% mutate(Date.Local = as.Date(Date.Local))
ggplot(data = fulldata) +
  geom_point(aes(x = Date.Local, y = O3.Mean)) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  scale_y_continuous(name = "O3") +
  ggtitle("Daily O3 mean") + 
  theme_bw()
```

## Missing data

- There are 52 time skips in the data, in a total of 473 days.
- The biggest skips is 108 days in 2011.
- The majority of skips are of 1 or 2 days.
- Around 9.5% missing data.
- The missing observations are distributed along the time without a clear pattern.

----

```{r}
dateJumps <- data$Date.Local - lag(data$Date.Local, n = 1)
dateJumps[is.na(dateJumps)] <- FALSE
ggplot(data = data[dateJumps > 1, ]) + 
  geom_point(aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  scale_x_date(name = "Date") + 
  scale_y_continuous(name = "O3") +
  ggtitle("Observations after data skips") +
  theme_bw()
```

----

```{r}
skips <- dateJumps[dateJumps > 1]
ggplot(data = as.data.frame(skips)) +
  geom_bar(aes(skips), fill = "blue") +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = "Quantity of days", breaks = seq(0, 150, 5)) +
  ggtitle("Days missing in sequence") +
  theme_bw()
```

## Imputation method

- It was used the kNN method to imputate values on missing observations.
- The kNN method needs the parameter k, the number of closest points considered.
- Starting with k = 7.

----

```{r}
imputedData <- data.frame("obs" = index(fulldata), "O3.Mean" = fulldata$O3.Mean)
imputedData <- kNN(imputedData, variable = "O3.Mean", k = 7) #K = 7 because we'll consider the week of the missing data
imputedData$Date.Local <- days

ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  ggtitle("Real vs Imputed data") +
  theme_bw()
```

----

- Method create a bad behavior when the size of the skips is bigger than 7 days.

```{r}
ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data") +
  theme_bw()
```


----

- To deal with this, the parameter k used for imputation will be different if the size of the skip is minor them 30 days, between 30 days and 100 days, or bigger than 100 days.
- k = 7, k = 45, k = 120, respectively.
- We will aggregate closest points by weighted by distance mean.

----

```{r}
imputedData[((is.na(fulldata$O3.Mean)) 
             & (fulldata$Date.Local > "2003-08-01") 
             & (fulldata$Date.Local < "2003-09-15")),"O3.Mean"] <- NA

imputedData[((is.na(fulldata$O3.Mean)) 
             & (fulldata$Date.Local > "2009-07-01") 
             & (fulldata$Date.Local < "2009-08-20")),"O3.Mean"] <- NA

imputedData <- kNN(imputedData, variable = c("O3.Mean"), numFun = weighted.mean, k = 45, weightDist = TRUE)


imputedData[((is.na(fulldata$O3.Mean)) 
              & (fulldata$Date.Local > "2011-01-01") 
              & (fulldata$Date.Local < "2012-01-01")),"O3.Mean"] <- NA

imputedData <- kNN(imputedData, variable = c("O3.Mean"), numFun = weighted.mean, k = 120, weightDist = TRUE)

ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  ggtitle("Real vs Imputed data (by separeted imput.)") +
  theme_bw()

```

----

```{r}
ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data (by separated imput.)") +
  theme_bw()
```

## Weekly data

```{r}
k = 0
val = 1
l = rep(NA, length(fulldata$Date.Local) - 1)
while(k < length(fulldata$Date.Local) - 1){
  for(i in 1:7){
    l[k+i] = val
  }
  val = val + 1
  k = k +7
}

weekData <- cbind(fulldata)
weekData$weekInd <- l
weekData <- weekData %>% group_by(weekInd) %>% summarise(O3.Mean = mean(O3.Mean, na.rm = TRUE))
weekData$Date.Local <- dayWeek
ggplot(weekData) +
  geom_point(aes(x = Date.Local, y = O3.Mean)) +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "2 year", name = "Date", date_labels  = "%Y") + 
  ggtitle("Weekly O3 mean") + 
  theme_bw()
```


----

- If the data is grouped by week, ignoring the missing values when aggregating, it'll have 33 missing observations.
- Around 4.3% missing data.

```{r}

temp <- weekData[!is.na(weekData$O3.Mean), ]
skips <- temp$Date.Local - lag(temp$Date.Local)
skips <- skips[skips > 7][-1]/7

ggplot(data = as.data.frame(skips)) +
  geom_bar(aes(skips), fill = "blue") +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = "Quantity of weeks", breaks = seq(0, 16, 2)) +
  ggtitle("Weeks missing in sequence") +
  theme_bw()
```

----

```{r}
imputedWeek <- cbind(weekData)
imputedWeek <- kNN(imputedWeek, variable = "O3.Mean", k = 4, 
                   numFun = weighted.mean, weightDist =  TRUE)

ggplot() +
  geom_point(data = imputedWeek[is.na(weekData$O3.Mean),], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = weekData, aes(x = Date.Local, y = O3.Mean), colour = "black", alpha = 0.4) +
  scale_x_date(name ="Date", date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(name = "O3") + 
  ggtitle("Weekly Real vs Imputed data") +
  theme_bw()
```

---=--

- It has the same problem when the sequence of missing data is to big.
- Again, if there is more than 5 missing weeks, it will be used k = 16, if it's less, it'll be k = 4.

```{r}
imputedWeek[(imputedWeek$Date.Local > "2011-01-01") 
            & (imputedWeek$Date.Local < "2012-01-01") 
            & (is.na(weekData$O3.Mean)),"O3.Mean"] <- NA

imputedWeek <- kNN(imputedWeek, variable = "O3.Mean", k = 16, 
                   numFun = weighted.mean, weightDist =  TRUE)

ggplot() +
  geom_point(data = imputedWeek[is.na(weekData$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = weekData, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date")+
  ggtitle("Weekly Real vs Imputed data") +
  theme_bw()
```

----

```{r}
ggplot() +
  geom_point(data = imputedWeek[is.na(weekData$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = weekData, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Weekly Real vs Imputed data") +
  theme_bw()
```

# Daily model

## Modelling process

- Metric to be minimized: MAE = $\dfrac{1}{n}\sum_n |y_t - \hat{y}_t|$.
- Rolling window of 2 years (730 days).
- Prediction of the next 7 days.
- First: Test if there is tendency with Wald-Wolfowitz runs test.
  - For every 2 years window, the p-value is smaller than $1e-3$.
- Second: Fitting of different models and evaluation of MAE error.

```{r}
trainDaily <- read.csv("trainDaily.csv")
trainDaily$Date.Local <- as.Date(trainDaily$Date.Local)
startDate <- trainDaily$Date.Local[1]
trainDailyZoo <- zoo(x = trainDaily$O3.Mean, order.by = trainDaily$Date.Local)
trainDaily.ts <- ts(data = trainDaily$O3.Mean, start = c(2001, as.numeric(format(startDate, "%j"))),
                   frequency = 365)
```

## Choice of models - trend

```{r}
plot(decompose(trainDaily.ts))
```

## Choice of models - ACF and PACF

```{r}
par(mfrow = c(1, 2))
acf(trainDaily.ts[1:730], main = "ACF on subset of train data", lag.max = 365)
pacf(trainDaily.ts[1:730], main = "PACF on subset of train data", lag.max = 365)
```

----

- Naive model: the next 7 days are predict as the mean of the last 4 weeks.
- Exponential smoothing forecast.
- Holt model with trend.
- ARMA(6,0) model.
- Auto ARIMA model.

----

- Process:
  - 1. For each model, run a 2 years window, for each:
    - Fit model.
    - Generate predictions of next 7 days.
    - Compute mean of residuals for that window.
  - 2. Compute MAE for model as the mean of residuals.
  
----

- Results for train data:
  - Auto ARIMA model: 0.005986731
  - Holt model: 0.006142857
  - SES model: 0.00617229
  - ARMA(6,0) model: 0.006279533
  - Naive model: 0.007498889

----

## Evaluating on test data

- MAE: 0.006503142

```{r}
testDaily <- read.csv("testDaily.csv")
testDaily$Date.Local <- as.Date(testDaily$Date.Local)
testDaily$month <- as.factor(format(testDaily$Date.Local, format = "%m"))
testDailyZoo <- zoo(x = testDaily$O3.Mean, order.by = testDaily$Date.Local)

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
```

# Weekly model

## Modelling process

- Metric to be minimized: MAE = $\dfrac{1}{n}\sum_n |y_t - \hat{y}_t|$.
- Rolling window of 2 years (104 weeks), by skiping 4 weeks.
- Prediction of the next 4 weeks.
- First: Test if there is tendency with Wald-Wolfowitz runs test.
  - For almost every 2 years window, the p-value is bigger than 0.1.
- Second: Fitting of different models and evaluation of MAE error.

```{r}
trainWeekly <- read.csv("trainWeekly.csv")
trainWeekly$Date.Local <- as.Date(trainWeekly$Date.Local)
startDate <- trainWeekly$Date.Local[1]
trainWeekly.ts <- ts(data = trainWeekly$O3.Mean, start = c(2001, format(startDate, "%U")), frequency = 52)
```

## Choice of models - trend

```{r}
plot(decompose(trainWeekly.ts))
```

## Choice of models - ACF and PACF

```{r}
par(mfrow = c(1, 2))
acf(trainWeekly.ts[1:104], main = "ACF on subset of train data", lag.max = 52)
pacf(trainWeekly.ts[1:104], main = "PACF on subset of train data", lag.max = 52)
```

----

- Baseline model: the next 4 weeks are predict as the mean of the last 4 weeks.
- Seasonal model: linear regression on seasonal dummies variable, each month is a factor.
- Linear model: linear regression on seasonal dummies and time index.
- Poly 2 model: linear regression on seasonal dummies and time index with degree 1 and 2.
- Poly 3 model: linear regression on seasonal dummies and time index with degree 1, 2, and 3.
- Holt Winters model without trend and with seasonality (multiplicative and additive).
- ARMA(1, 0) model.

----

- Process:
  - 1. For every 2 years window:
    - Fit all the models.
    - Generate predictions of next 4 weeks.
  - 2. With predictions for every week, compute residuals $r_t = y_t - \hat{y}_t$.
  - 3. With residuals, compute MAE.
    - We group the predictions by each window, in this subsets, we compute the MAE of the 4 weeks predicted, than, we compute the mean of the MAE for all windows.
    - We also group the predictions by the numbers of weeks after the last observation, that range from 1 to 4, and compute the MEAN for each of this subset.
  
----

- Results:
  - Seasonal: 0.003887602
  - Linear: 0.004033049
  - Poly 2: 0.004148709
  - Arma(1, 0): 0.004695954
  - Poly 3: 0.004760530
  - HoltWinters additive: 0.005016573
  - HoltWinters multiplicative: 0.005106128
  - Baseline: 0.005218411
  
----

```{r}
#-------
#calculating residuals
residuals <- read.csv("auxRes.csv")
library(dplyr)
aux <- function(x){
  return(mean(abs(x)))
}

#residuals by day prediction
residuals.mae <- residuals %>% group_by(day) %>% summarise_all(aux)
residuals.mae[, c(-10, -11)]

```

## Residuals

```{r}
auxPred <- read.csv("auxPred.csv")
acf(auxPred$sazonal, main = "ACF of Sazonal model residuals on training data ", xlim = c(0, 52), lag.max = 52)
```

----

```{r}
qqPlot(auxPred$sazonal, main = "QQPlot of sazonal model residuals", ylab = "Residual")
```

----

```{r}
acf(auxPred$linear, main = "ACF of Linear model residuals on training data", xlim = c(0, 52), lag.max=  52)
```


----

```{r}
qqPlot(auxPred$linear, main = "QQPlot of linear model residuals", ylab = "Residual")
```


## Evaluating on test data

- MAE:  0.003476891
- MAE by day: 1 - 0.003450016; 2 - 0.003388719; 3 - 0.003502532; 4 - 0.003566297

```{r}
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
```
