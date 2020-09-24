library(ggplot2)
library(dplyr)
library(VIM)
library(zoo)
data <- read.csv("NY.csv")
data <- data %>% select(Date.Local, O3.Mean) %>% mutate(Date.Local = as.Date(Date.Local))
#fulldata have NA rows in days that there is no observation
fulldata <- data.frame(Date.Local = seq(from = min(data$Date.Local), to = max(data$Date.Local), by = 1))
fulldata <- as.data.frame(merge(data, fulldata, all.y = TRUE)) %>% mutate(Date.Local = as.Date(Date.Local))
ggplot(data = fulldata) +
  geom_point(aes(x = Date.Local, y = O3.Mean)) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  scale_y_continuous(name = "O3") +
  ggtitle("Daily O3 mean") + 
  theme_bw()

print(min(data$Date.Local))
print(max(data$Date.Local))
#About 15 years of data

#locate where is missing data
dateJumps <- data$Date.Local - lag(data$Date.Local, n = 1)
dateJumps[is.na(dateJumps)] <- FALSE
sum(dateJumps > 1)
#There is 52 date jumps
print(as.double(sum(dateJumps[dateJumps > 1]))/length(data$Date.Local))
#Around 9.5% of the data is missing

ggplot(data = data[dateJumps > 1, ]) + 
  geom_point(aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  scale_x_date(name = "Date") + 
  scale_y_continuous(name = "O3") +
  ggtitle("Observations after data skips") +
  theme_bw()

skips <- dateJumps[dateJumps > 1]
ggplot(data = as.data.frame(skips)) +
  geom_bar(aes(skips), fill = "blue") +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = "Quantity of days", breaks = seq(0, 150, 5)) +
  ggtitle("Days missing in sequence") +
  theme_bw()

#Can't see a pattern in skips
#A Comparison of Various Imputation Methods for Missing
#Values in Air Quality Data
#kNN function can't recieve a date variable
imputedData <- data.frame("obs" = index(fulldata), "O3.Mean" = fulldata$O3.Mean)
imputedData <- kNN(imputedData, k = 7) #K = 7 because we'll consider the week of the missing data
imputedData$Date.Local <- fulldata$Date.Local

ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  ggtitle("Real vs Imputed data") +
  theme_bw()

ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data") +
  theme_bw()

#There is a problem in the year of 2011 because there is more than 100 days
#missing in sequence
#I'm going to use different knn imputations for this year and for the rest of the data
imputedData2 <- data.frame("obs" = index(fulldata), "O3.Mean" = imputedData$O3.Mean)


imputedData2[((is.na(fulldata$O3.Mean)) 
              & (fulldata$Date.Local > "2011-01-01") 
              & (fulldata$Date.Local < "2012-01-01")),"O3.Mean"] <- NaN

imputedData3 <- cbind(imputedData2)
imputedData2 <- kNN(imputedData2, variable = c("O3.Mean"), k = 120, weightDist = FALSE)
imputedData3 <- kNN(imputedData3, variable = c("O3.Mean"), numFun = weighted.mean, k = 120, weightDist = TRUE)
imputedData2$Date.Local <- fulldata$Date.Local
imputedData3$Date.Local <- fulldata$Date.Local


ggplot() +
  geom_point(data = imputedData2[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data (by separated imput.)") +
  theme_bw()

ggplot() +
  geom_point(data = imputedData3[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data (by separated imput., weighted by distance)") +
  theme_bw()

#Creating weekly data
weekData <- cbind(fulldata)
weekData$weekday <- format(fulldata$Date.Local, format = "%Y-%U")