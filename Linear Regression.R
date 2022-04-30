# LINEAR REGRESSION FORECASTING

# Model 1 (Trend)
data.trend <- tslm(data2.ts[,3] ~ trend)
summary(data.trend)
data.for <- forecast(data.trend, h = 12)
plot(data.for)
lines(fitted(data.trend), col="blue")        # fitted() extracts fitted values to x,y
lines(data1.fit$time.series[,2], col = "red")
lines(data1.ts[,3])

# Model 2 (Trend + season)
data.trend <- tslm(data2.ts[,3] ~ trend + season)
summary(data.trend)
data.for <- forecast(data.trend, h = 12)
plot(data.for, 
     main = "Trend + Season Linear Regression Model", ylab="Concentration (ug/m^3)", xlab ="Year",)
lines(fitted(data.trend), col="blue")        # fitted() extracts fitted values to x,y
lines(data1.fit$time.series[,2], col = "red")
lines(data1.ts[,3])

#Model 2 error calculation
b_error <- data.for$mean - data3.ts[,3]
rmse(b_error)
mae(b_error)


#Create PM10 time series for Model 3
data.pm10 <- data.frame(AllData$No , AllData$year, AllData$month, 
                     AllData$PM10, AllData$station)
colnames(data.pm10) <- c("No", "year", "month", "PM10", "station")
data.pm10 <-na.omit(data.pm10)

#Split subset into PM10 training and testing data
data.pm10.tr <- subset(data.pm10, No <= 26304, station = "Wanliu")
data.pm10.te <- subset(data.pm10, No > 26304, station = "Wanliu")

#PM10 training data
data4 <-data.pm10.tr %>% group_by(year,month) %>% summarize(Avg = mean(PM10))

data4.ts <- ts(data4, start=c(2013, 3), end=c(2016, 2), 
               frequency=12)
data4.ts

data4.fit <- stl(data4.ts[,3], s.window="periodic")


#PM10 testing data
data5 <-data.pm10.te %>% group_by(year,month) %>% summarize(Avg = mean(PM10))

data5.ts <- ts(data5, start=c(2016, 3), end=c(2017, 2), 
               frequency=12)
data5.ts



# Model 3 (Trend + season + PM10)
PM10 <- data4.ts[,3]
data.reg <- tslm(data2.ts[,3] ~ trend + season + PM10)
summary(data.reg)

PM10 <- data5.ts[,3]
data.for <- forecast(data.reg, newdata = data.frame(PM10))

plot(data.for, 
     main = "Trend + Season Linear + P10 Regression Model", ylab="Concentration (ug/m^3)", xlab ="Year")
lines(fitted(data.reg), col="blue")        # fitted() extracts fitted values to x,y
lines(data1.fit$time.series[,2], col = "red")
lines(data1.ts[,3])

#Model 3 error calculation
b_error <- data.for$mean - data3.ts[,3]
rmse(b_error)
mae(b_error)
