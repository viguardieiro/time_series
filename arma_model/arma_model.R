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

plot_acf_pacf(X[[1]])

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

data.frame(mean = mean(X[[3]]), variance = var(X[[3]])*296/297)

model3 <- rnorm(297)
plot(X[[3]], main = "Model for Series 3", ylab = "Z_t")
lines(model3, col = "red")

model3$residual <- X[[3]] - model3
plot_res(model3$residual)


#SERIES 4
plot(X[[4]], main = "Series 4", ylab = "Z_t")
plot_acf_pacf(X[[4]])

#LOOK LIKES MA(2)

model4 <- arma(X[[4]], order = c(0, 2))
summary(model4)

plot(X[[4]], main = "Model for Series 4", ylab = "Z_t")
lines(model4$fitted.values, col = "red")

plot_res(model4$residuals)

#SERIES 5
plot(X[[5]], main = "Series 5", ylab = "Z_t")
plot_acf_pacf(X[[5]])

model5 <- arma(X[[5]], order = c(2, 0))
summary(model5)

plot(X[[5]], main = "Model for Series 5 AR(2)", ylab = "Z_t")
lines(model5$fitted.values, col = "red")

model5 <- arma(X[[5]], order = c(2, 2))
summary(model5)

plot(X[[5]], main = "Model for Series 5 ARMA(2, 2)", ylab = "Z_t")
lines(model5$fitted.values, col = "red")

plot_res(model5$residuals)