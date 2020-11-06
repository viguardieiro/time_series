library(tseries)
library(fpp2)

data <- debitcards
plot(data, main = 'Retail Debit Card Usage In Iceland')

#The data doesn't have stable variance
lambda <- BoxCox.lambda(data)
data.stabilized <- BoxCox(data, lambda)
plot(data.stabilized, 
     main = 'Retail Debit Card Usage In Iceland after BoxCox', 
     ylab = 'Value')

#Removing sazonality
plot(decompose(data.stabilized))
data.trend <- data.stabilized - decompose(data.stabilized)$seasonal
plot(data.trend)

#Testing if is starionary
adf.test(data.trend)
#P-value is not lower than 0.05

#Testing diff 1
adf.test(diff(data.trend))
#P-value is lower than 0.05

plot(diff(data.trend), 
     main = 'Retail Debit Card Usage In Iceland after BoxCox (diff 1)', 
     ylab = 'Value')

#Looking at the ACF and PACF
par(mfrow = c(2, 1))
acf(diff(data.trend))
pacf(diff(data.trend))

#Look likes AR(2) or some ARMA
