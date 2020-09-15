library(tseries)
library(zoo)
load("dados_arma_4.RData")

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


# SERIES 1
plot(X[[1]], main = "Series 1", ylab = "Z_t")
par(mfrow = c(2, 1))
acf(X[[1]], main = "ACF")
pacf(X[[1]], main = "PACF")

#LOOKS LIKE AR(2)

model1 <- arma(X[[1]], order = c(2, 0))

plot(X[[1]], main = "Model for Series 1", ylab = "Z_t")
lines(model1$fitted.values, col = "red")

plot_res(model1$residuals)


#SERIES 2
plot(X[[2]], main = "Series 2", ylab = "Z_t")
plot_acf_pacf(X[[2]])


#LOOKS LIKE MA(2)

model2 <- arma(X[[2]], order = c(0, 2))
summary(model2)


plot(X[[2]], main = "Model for Series 2", ylab = "Z_t")
lines(model2$fitted.values, col = "red")

plot_res(model2$residuals)

#SERIES 3
plot(X[[3]], main = "Series 3", ylab = "Z_t")
plot_acf_pacf(X[[3]])

#LOOKS LIVE ARMA(0, 0) WHITE NOISE

model3 <- arima(X[[3]], order = c(0, 0, 0), include.mean = FALSE)
summary(model3)
plot(model3$coef)
