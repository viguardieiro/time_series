library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)

data <- read.csv("norway_new_car_sales_by_make.csv")
car_brand <- "Volkswagen"
data <- data %>% dplyr::filter(data$Make == car_brand)

carSales <- zooreg(data$Quantity, frequency =  12, start = c(2007, 1))
plot(carSales)

ModelMAPE <- NA
trainSales <- vector("list", 96)
testSales <- NA
startDate <- as.yearmon("2007-01-01")
endDate <- as.yearmon("2008-12-01")
finalDate <- as.yearmon("2017-01-01")
i <- 1
while(endDate < finalDate){
  trainSales[[i]] <- window(carSales, start = startDate, end=endDate)
  startDate <- startDate + 1/12
  endDate <- endDate + 1/12
  testSales[i] <- window(carSales, start = endDate, end = endDate)
  i <- i +1
}


#polynomial tendency
t <- rep(1:length(trainSales[[1]]))
predictions1 <- NA
predictions2 <- NA
predictions3 <- NA
for(n in seq_along(trainSales)){
  fit1 <- lm(trainSales[[n]]~t)
  fit2 <- lm(trainSales[[n]]~poly(t,2))
  fit3 <- lm(trainSales[[n]]~poly(t,3))
  predictions1[n] <- predict(fit1)[1]
  predictions2[n] <- predict(fit2)[1]
  predictions3[n] <- predict(fit3)[1]
}

MAPE <- mean(abs((testSales-predictions1)/testSales))
ModelMAPE["Poly1T"] <- MAPE
MAPE <- mean(abs((testSales-predictions2)/testSales))
ModelMAPE["Poly2T"] <- MAPE
MAPE <- mean(abs((testSales-predictions3)/testSales))
ModelMAPE["Poly3T"] <- MAPE

# visualization of fitted model
predictions <- NA
for(i in seq_along(trainSales)){
  fit <- lm(trainSales[[i]]~poly(t, 3))
  predictions[i] <- predict(fit)[1]
}
plot(x = index(carSales)[(121-96):121], y = predictions, type = 'l', 
     col = 'red', xlab =  "Data", ylab = "Sales")
lines(carSales)

### DUMMY SAZONAL
Q <- factor(rep(1:12, length.out = 121))
predictions <- NA
for(n in seq_along(trainSales)){
  Qn <- Q[n:(n+23)]
  fit <- lm(trainSales[[n]]~Qn+poly(t,3))
  predictions[n] <- predict(fit)[1]
}
MAPE <- mean(abs((testSales-predictions)/testSales))
ModelMAPE["SazonalDummy"] <- MAPE


#SIMPLE EXPONENTIAL SMOOTHING
#TUNNING ALPHA PARAMETER
alpha <- seq(0.05, 0.95, by = .05)
MAPE <- NA
for(i in seq_along(alpha)){
  predictions <- NA
  print(i)
  for(j in seq_along(trainSales)){
    fit <- ses(trainSales[[j]], alpha = alpha[i], h = 1)
    predictions[j] <- fit$mean[1]
  }
  MAPE[i] <- mean(abs((testSales-predictions)/testSales))
}

alpha.err <- data_frame(alpha, MAPE)
alpha.min <- filter(alpha.err, MAPE == min(MAPE))
ModelMAPE["ses"] <- alpha.min$MAPE
ggplot() +
  geom_line(data = alpha.err, aes(x = alpha, y = MAPE)) +
  geom_point(data = alpha.min, aes(x = alpha, y = MAPE), color = 'blue')

# visualization of fitted model
predictions <- NA
for(i in seq_along(trainSales)){
  fit <- ses(trainSales[[i]], alpha = 0.3, h = 1)
  predictions[i] <- fit$mean[1]
}
plot(x = index(carSales)[(121-96):121], y = predictions, type = 'l', 
     col = 'red', xlab =  "Data", ylab = "Sales")
lines(carSales)

#HOLT'S METHOD
#TUNNING ALPHA AND BETA PARAMETER
betap <- seq(0.01, 0.3, by = 0.01)
MAPE <- NA
for(k in seq_along(betap)){
  predictions <- NA
  for(j in seq_along(trainSales)){
    fit <- holt(trainSales[[j]], beta = betap[k], h = 1)
    predictions[j] <- fit$mean[1]
  }
  MAPE[k] <- mean(abs((testSales-predictions)/testSales))   
}

beta.err <- data_frame(betap, MAPE)
beta.min <- filter(beta.err, MAPE == min(MAPE))
ModelMAPE["HoltAdd"] <- beta.min$MAPE

ggplot() +
  geom_line(data = beta.err, aes(x = betap, y = MAPE)) +
  geom_point(data = beta.min, aes(x = betap, y = MAPE), color = 'blue')

# visualization of fitted model
predictions <- NA
for(i in seq_along(trainSales)){
  fit <- holt(trainSales[[i]], beta = 0.06, h = 1)
  predictions[i] <- fit$mean[1]
}
plot(fit, xlim = c(as.yearmon("2009-01-01"), finalDate), ylim = c(0, 2800),
    xlab = "Data", ylab = "Sales")
lines(x = index(carSales)[(121-96):121], y = predictions, type = 'l', 
     col = 'red')
lines(carSales)

#MULTIPLICATIVE METHOD
betap <- seq(0.01, 0.3, by = 0.01)
MAPE <- NA
for(k in seq_along(betap)){
  predictions <- NA
  for(j in seq_along(trainSales)){
    fit <- holt(trainSales[[j]], beta = betap[k], h = 1, type = 'multiplicative')
    predictions[j] <- fit$mean[1]
  }
  MAPE[k] <- mean(abs((testSales-predictions)/testSales))   
}

beta.err <- data_frame(betap, MAPE)
beta.min <- filter(beta.err, MAPE == min(MAPE))
ModelMAPE["HoltMult"] <- beta.min$MAPE

ggplot() +
  geom_line(data = beta.err, aes(x = betap, y = MAPE)) +
  geom_point(data = beta.min, aes(x = betap, y = MAPE), color = 'blue')

# visualization of fitted model
predictions <- NA
for(i in seq_along(trainSales)){
  fit <- holt(trainSales[[i]], beta = 0.06, h = 1)
  predictions[i] <- fit$mean[1]
}
plot(fit, xlim = c(as.yearmon("2009-01-01"), endDate), 
     xlab = "Data", ylab = "Sales", ylim = c(0, 2800))
lines(x = index(carSales)[(121-96):121], y = predictions, type = 'l', col = 'red')
lines(carSales)

#HOLTWINTER METHOD
autoModel <- HoltWinters(carSales)
print(autoModel)
plot(forecast(autoModel))

hwalpha <- seq(0.2, 0.5, length.out = 8)
hwbeta <- seq(0.001, 0.034, length.out = 8)
hwgamma <- seq(0.01, 0.5, length.out = 8)

MAPE <- array(dim = c(8, 8, 8))
for(i in seq_along(hwalpha)){
  for(j in seq_along(hwbeta)){
    for(k in seq_along(hwgamma)){
      print(c(i, j, k))
      predictions <- NA
      for(n in seq_along(trainSales)){
        fit <- HoltWinters(trainSales[[n]], alpha = hwalpha[i], 
                           beta = hwbeta[j], gamma = hwgamma[k])
        predictions[n] <- predict(fit, n.ahead = 1)[1]
      }
      MAPE[i,j,k] <- mean(abs((testSales-predictions)/testSales))
    }
  }
}

MAPEmin <- 10000
bestParams <- NA
for(i in seq_along(hwalpha)){
  for(j in seq_along(hwbeta)){
    for(k in seq_along(hwgamma)){
      if(MAPEmin > MAPE[i, j, k]){
        MAPEmin <- MAPE[i, j, k]
        bestParams <- c(hwalpha[i], hwbeta[j], hwgamma[k])
      }
    }
  }
}

#estimated values for final model
# visualization of fitted model
predictions <- NA
for(i in seq_along(trainSales)){
  fit <- HoltWinters(trainSales[[i]], alpha = bestParams[1], 
                     beta = bestParams[2], gamma = bestParams[3])
  predictions[i] <- predict(fit, n.ahead = 1)[1]
}

plot(forecast(fit), xlim = c(as.yearmon("2009-01-01"), endDate + 1/4), 
     xlab = "Data", ylab = "Sales", ylim = c(0, 2800))
lines(x = index(carSales)[(121-96):121], y = predictions, type = 'l', col = 'red')
lines(carSales)




