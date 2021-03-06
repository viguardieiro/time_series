## ---------------------------------------------------------------------------------------
library(tseries)
load("novos_dados_arma_4.RData")

plot_acf_pacf <- function(series){
  par(mfrow = c(2, 1))
  acf(series, main = "ACF")
  pacf(series, main = "PACF")
}

plot_res <- function(res){
  plot(res, main = "Residuals of model",
       ylab = "Residual", type = "p")
  abline(0, 0)
}


## ---------------------------------------------------------------------------------------
#SERIES 1
plot(X[[1]], main = "Series 1", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[1]])

#LOOKS LIVE AR(2) OR ARMA(2, 2)

## ---------------------------------------------------------------------------------------
model1 <- arma(X[[1]], order = c(2, 0))
summary(model1)


## ---------------------------------------------------------------------------------------
plot(X[[1]], main = "Model for Series 1 AR(2)", ylab = "Z_t")
lines(model1$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
model1 <- arma(X[[1]], order = c(2, 2))
summary(model1)

## ---------------------------------------------------------------------------------------
plot(X[[1]], main = "Model for Series 1 ARMA(2, 2)", ylab = "Z_t")
lines(model1$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model1$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 2
plot(X[[2]], main = "Series 2", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[2]])


## ---------------------------------------------------------------------------------------
model2 <- arma(X[[2]], order = c(0, 2))
summary(model2)

#LOOK LIKES MA(2) OR ARMA(2, 2)


## ---------------------------------------------------------------------------------------
plot(X[[2]], main = "Model for Series 2 MA(2)", ylab = "Z_t")
lines(model2$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
model2 <- arma(X[[2]], order = c(2, 2))
summary(model2)


## ---------------------------------------------------------------------------------------
plot(X[[2]], main = "Model for Series 2 ARMA(2, 2)", ylab = "Z_t")
lines(model2$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model2$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 3
plot(X[[3]], main = "Series 3", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[3]])

#LOOK LIKES WHITE NOISE

## ---------------------------------------------------------------------------------------
data.frame(mean = mean(X[[3]]), variance = var(X[[3]])*296/297)


## ---------------------------------------------------------------------------------------
model3 <- rnorm(297)
plot(X[[3]], main = "Model for Series 3", ylab = "Z_t")
lines(model3, col = "red")


## ---------------------------------------------------------------------------------------
model3$residual <- X[[3]] - model3
plot_res(model3$residual)


## ---------------------------------------------------------------------------------------
#SERIES 4
plot(X[[4]], main = "Series 4", ylab = "Z_t")

## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[4]])

#LOOK LIKES MA(2)
## ---------------------------------------------------------------------------------------
model4 <- arma(X[[4]], order = c(0, 2))
summary(model4)

## ---------------------------------------------------------------------------------------
plot(X[[4]], main = "Model for Series 4 MA(2)", ylab = "Z_t")
lines(model4$fitted.values, col = "red")

## ---------------------------------------------------------------------------------------
plot_res(model4$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 5
plot(X[[5]], main = "Series 5", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[5]])

#LOOK LIKES AR(2) OR ARMA(2, 2)

## ---------------------------------------------------------------------------------------
model5 <- arma(X[[5]], order = c(2, 0))
summary(model5)


## ---------------------------------------------------------------------------------------
plot(X[[5]], main = "Model for Series 5 AR(2)", ylab = "Z_t")
lines(model5$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
model5 <- arma(X[[5]], order = c(2, 2))
summary(model5)


## ---------------------------------------------------------------------------------------
plot(X[[5]], main = "Model for Series 5 ARMA(2, 2)", ylab = "Z_t")
lines(model5$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model5$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 6
plot(X[[6]], main = "Series 6", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[6]])

#LOOK LIKES MA(1)

## ---------------------------------------------------------------------------------------
model6 <- arma(X[[6]], order = c(0, 1))
summary(model6)


## ---------------------------------------------------------------------------------------
plot(X[[6]], main = "Model for Series 6 MA(1)", ylab = "Z_t")
lines(model6$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model6$residuals)


## ---------------------------------------------------------------------------------------
hist(model6$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 7
plot(X[[7]], main = "Series 7", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[7]])

#LOOK LIKES MA(1)

## ---------------------------------------------------------------------------------------
model7 <- arma(X[[7]], order = c(0, 1))
summary(model7)


## ---------------------------------------------------------------------------------------
plot(X[[7]], main = "Model for Series 7 MA(1)", ylab = "Z_t")
lines(model7$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model7$residuals)


## ---------------------------------------------------------------------------------------
hist(model7$residuals)


## ---------------------------------------------------------------------------------------
#SERIES 8
plot(X[[8]], main = "Series 8", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[8]])
#LOOK LIKES WHITE NOISE


## ---------------------------------------------------------------------------------------
data.frame(mean = mean(X[[8]]), variance = var(X[[8]])*227/228)


## ---------------------------------------------------------------------------------------
model8 <- rnorm(228)
plot(X[[8]], main = "Model for Series 8", ylab = "Z_t")
lines(model8, col = "red")


## ---------------------------------------------------------------------------------------
model8$residual <- X[[8]] - model8
plot_res(model8$residual)


## ---------------------------------------------------------------------------------------
hist(model8$residual)


## ---------------------------------------------------------------------------------------
#SERIES 9
plot(X[[9]], main = "Series 9", ylab = "Z_t")


## ---------------------------------------------------------------------------------------
plot_acf_pacf(X[[9]])

#LOOK LIKES MA(1) OR ARMA(1, 1)

## ---------------------------------------------------------------------------------------
model9 <- arma(X[[9]], order = c(0, 1))
summary(model9)


## ---------------------------------------------------------------------------------------
plot(X[[9]], main = "Model for Series 9 MA(1)", ylab = "Z_t")
lines(model9$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
model9_2 <- arma(X[[9]], order = c(1, 1))
summary(model9_2)


## ---------------------------------------------------------------------------------------
plot(X[[9]], main = "Model for Series 9 ARMA(1,1)", ylab = "Z_t")
lines(model9_2$fitted.values, col = "red")


## ---------------------------------------------------------------------------------------
plot_res(model9_2$residuals)


## ---------------------------------------------------------------------------------------
hist(model9_2$residuals)

