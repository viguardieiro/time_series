library(ggplot2)
library(dplyr)
library(VIM)
library(zoo)
data <- read.csv("NY.csv")
data <- data %>% select(Date.Local, O3.Mean) %>% mutate(Date.Local = as.Date(Date.Local))
data <- data[-1, ]
startDate <- data$Date.Local[1] #First observed sunday
finalDate <- data$Date.Local[length(data$Date.Local)] #Last observed day
#About 15 years of data
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
imputedData <- kNN(imputedData, variable = "O3.Mean", k = 7, numFun = weighted.mean, weightDist = TRUE) #K = 7 because we'll consider the week of the missing data
imputedData$Date.Local <- days

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

#bigger than 30 days missing
imputedData[((is.na(fulldata$O3.Mean)) 
             & (fulldata$Date.Local > "2003-08-01") 
             & (fulldata$Date.Local < "2003-09-15")),"O3.Mean"] <- NA

imputedData[((is.na(fulldata$O3.Mean)) 
             & (fulldata$Date.Local > "2009-07-01") 
             & (fulldata$Date.Local < "2009-08-20")),"O3.Mean"] <- NA

imputedData <- kNN(imputedData, variable = c("O3.Mean"), numFun = weighted.mean, k = 45, weightDist = TRUE)


#bigger than 100 days missing
imputedData[((is.na(fulldata$O3.Mean)) 
              & (fulldata$Date.Local > "2011-01-01") 
              & (fulldata$Date.Local < "2012-01-01")),"O3.Mean"] <- NA

imputedData <- kNN(imputedData, variable = c("O3.Mean"), numFun = weighted.mean, k = 120, weightDist = TRUE)


ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date") +
  ggtitle("Real vs Imputed data (by separated imput.)") +
  theme_bw()

ggplot() +
  geom_point(data = imputedData[is.na(fulldata$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = fulldata, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Real vs Imputed data (by separated imput.)") +
  theme_bw()




#############################3
### WEEK DATA

k = 0
val = 1
l = rep(NA, length(fulldata$Date.Local) - 1)
while(k < length(fulldata$Date.Local) - 1){
  for(i in 1:7){
    print(val)
    l[k+i] = val
  }
  val = val + 1
  k = k +7
}
#Creating weekly data
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

#look for missing data
print(sum(is.na(weekData$O3.Mean)))
print(sum(is.na(weekData$O3.Mean))/length(weekData$O3.Mean))
#around 4.3% of the data is missing

temp <- weekData[!is.na(weekData$O3.Mean), ]
skips <- temp$Date.Local - lag(temp$Date.Local)
skips <- skips[skips > 7][-1]/7

ggplot(data = as.data.frame(skips)) +
  geom_bar(aes(skips), fill = "blue") +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = "Quantity of weeks", breaks = seq(0, 16, 2)) +
  ggtitle("Weeks missing in sequence") +
  theme_bw()

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

#Imputing by different
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

ggplot() +
  geom_point(data = imputedWeek[is.na(weekData$O3.Mean), ], aes(x = Date.Local, y = O3.Mean), colour = 'red') +
  geom_point(data = weekData, aes(x = Date.Local, y = O3.Mean), alpha = 0.2, fill = "black") +
  scale_y_continuous(name = "O3") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", name = "Date", limits = c(as.Date("2011-01-01"), as.Date("2012-01-01"))) +
  ggtitle("Weekly Real vs Imputed data") +
  theme_bw()

## Separating between training and test data
startLastYear <- days[length(days) - (365*3)]
trainDaily <- imputedData[imputedData$Date.Local < startLastYear, c("O3.Mean", "Date.Local")]
testDaily <- imputedData[imputedData$Date.Local > startLastYear, c("O3.Mean", "Date.Local")]
write.csv(trainDaily, "trainDaily.csv", row.names = FALSE)
write.csv(trainDaily, "testDaily.csv", row.names = FALSE)
trainWeekly <- imputedWeek[imputedWeek$Date.Local < startLastYear, c("O3.Mean", "Date.Local")]
trainWeekly[268,] <- NA
trainWeekly[599,] <- NA
trainWeekly <- na.omit(trainWeekly)
testWeekly <- imputedWeek[imputedWeek$Date.Local > startLastYear, c("O3.Mean", "Date.Local")]
write.csv(trainWeekly, "trainWeekly.csv", row.names = FALSE)
write.csv(trainWeekly, "testWeekly.csv", row.names = FALSE)
