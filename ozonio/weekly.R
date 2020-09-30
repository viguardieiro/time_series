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
naive <- rep(NA, 153)
sazonal.linear <- rep(NA, 153)
sazonal.poly2 <- rep(NA, 153)
sazonal.poly3 <- rep(NA, 153)
holt.d <- rep(NA, 153)
holt.w_add <- rep(NA, 153)
holt.w_mul <- rep(NA, 153)
predictions <- data.frame()

for(i in seq(1, 611, 4)){
  ii = (i -1)/4 + 1
  #variables
  o3 <- trainWeekly$O3.Mean[i:(i+103)]
  startDate <- trainWeekly$Date.Local[i]
  o3.ts <- ts(o3, frequency = 52, start = c(format(startDate, "%Y"), format(startDate, "%U")))
  ind <- trainWeekly$ind[i:(i+103)]
  month <- trainWeekly$month[i:(i+103)]
  
  #models
  #linear regression
  model.sazonal_linear <- lm(o3~ind+month)
  model.sazonal_poly2 <- lm(o3~poly(ind, 2) + month)
  model.sazonal_poly3 <- lm(o3~poly(ind, 3) + month)
  
  optim_holt <- function(p){
    model <- holt(o3.ts, h = 4, alpha = p[1], beta = p[2])
    pred <- coredata(model$mean)
    mae <- sum(abs(pred - realO3))
    return(mae)
  }
  
  #param <- optim(c(0.1, 0.1), fn = optim_holt, gr = "L-BFGS-B")
  #holt method
  model.holt <- holt(o3.ts, h = 4)
  
  #holt winters
  model.hw_add <- HoltWinters(o3.ts, seasonal = "additive")
  model.hw_mul <- HoltWinters(o3.ts, seasonal = "mult")
  
  newdata = data.frame(ind = (i+104):(i+107), 
                        month = trainWeekly$month[(i+104): (i+107)])
  
  #prediction
  pred.naive <- rep(mean(o3[(length(o3)-4):length(o3)]), 4)
  pred.linear <- predict(model.sazonal_linear, newdata = newdata)
  pred.poly_2 <- predict(model.sazonal_poly2, newdata = newdata)
  pred.poly_3 <- predict(model.sazonal_poly3, newdata = newdata)
  pred.holt <- coredata(model.holt$mean)
  pred.hw_add <- coredata(forecast(model.hw_add, h = 4)$mean)
  pred.hw_mul <- coredata(forecast(model.hw_mul, h = 4)$mean)
  
  #updating dataframe of predictions  
  temp <- data.frame(Date.Local = trainWeekly$Date.Local[(i+104): (i+107)],
                     pred.naive = pred.naive,
                     pred.linear = pred.linear,
                     pred.poly_2 = pred.poly_2,
                     pred.poly_3 = pred.poly_3,
                     pred.holt = pred.holt,
                     pred.hw_add = pred.hw_add,
                     pred.hw_mul = pred.hw_mul,
                     real = trainWeekly$O3.Mean[(i+104): (i+107)])
  
  predictions <- rbind(predictions, temp)
}

residuals <- data.frame(naive = predictions$pred.naive - predictions$real,
                        linear = predictions$pred.linear - predictions$real,
                        poly_2 = predictions$pred.poly_2- predictions$real,
                        poly_3 = predictions$pred.poly_3- predictions$real,
                        holt = predictions$pred.holt- predictions$real,
                        hw_add = predictions$pred.hw_add- predictions$real,
                        hw_mul = predictions$pred.hw_mul - predictions$real)
aux <- function(x){
  return(mean(abs(x)))
}
sort(sapply(residuals, aux))

plot(acf(residuals$linear), main = "Linear model  residuals ACF")
qqPlot(residuals$linear, main = "QQPlot of linear model residuals", ylab = "Residual")

plot(acf(residuals$poly_2), main = "Linear model  residuals ACF")
qqPlot(residuals$poly_2, main = "QQPlot of linear model residuals", ylab = "Residual")


#working if differenced series
trainWeekly$O3.diff <- c(NA,diff(trainWeekly$O3.Mean))
trainWeekly.ts_diff <- diff(trainWeekly.ts)
plot(decompose(trainWeekly.ts_diff))

