library(ggplot2)
library(dplyr)
library(VIM)
library(zoo)
data <- read.csv("NY.csv")
data <- data %>% select(Date.Local, O3.Mean) %>% mutate(Date.Local = as.Date(Date.Local))
str(data)
ggplot(data = data) +
  geom_point(aes(x = Date.Local, y = O3.Mean)) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  scale_y_continuous(name = "O3") +
  ggtitle("Daily O3 mean") + 
  theme_bw()

print(min(data$Date.Local))
print(max(data$Date.Local))
#About 15 years of data

#check if there is missing data
dateJumps <- data$Date.Local - lag(data$Date.Local, n = 1)
dateJumps[is.na(dateJumps)] <- FALSE
sum(dateJumps > 1)
#There is 15 date jumps
print(as.double(sum(dateJumps[dateJumps > 1]))/length(data$Date.Local))
#Around 9.5% of the data is missing

ggplot(data = data[dateJumps > 1, ]) + 
  geom_point(aes( x = Date.Local, y = O3.Mean), colour = 'red') +
  scale_x_date(name = "Date") + 
  scale_y_continuous(name = "O3") +
  ggtitle("Observations after data skips") +
  theme_bw()

skips <- dateJumps[dateJumps > 1]
ggplot(data = as.data.frame(skips)) +
  geom_bar(aes(skips), fill = "blue") +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = "Days missing in sequence") +
  ggtitle("Days missing in sequence") +
  theme_bw()

#Can't see a pattern in skips
#A Comparison of Various Imputation Methods for Missing
#Values in Air Quality Data
dataNaN <- data.frame(Date.Local = seq(from = min(data$Date.Local), to = max(data$Date.Local), by = 1))
dataNaN <- as.data.frame(merge(data, dataNaN, all.y = TRUE))
imputedData <- kNN(dataNaN)
