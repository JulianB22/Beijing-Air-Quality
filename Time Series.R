#Remove irrelevant variables
data.z <- data.frame(AllData$No , AllData$year, AllData$month, 
                     AllData$PM2.5, AllData$station)
colnames(data.z) <- c("No", "year", "month", "PM2.5", "station")
data.z <-na.omit(data.z)

#Split into training data and testing data
data.tr <- subset(data.z, No <= 26304)
data.te <- subset(data.z, No > 26304)


#TIME SERIES FORECASTING

#All data time series
data1 <-data.z %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5))

data1.ts <- ts(data1, start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
data1.ts

data1.fit <- stl(data1.ts[,3], s.window="periodic")
plot (data1.fit)

#Training data time series
data2 <-data.tr %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5))

data2.ts <- ts(data2, start=c(2013, 3), end=c(2016, 2), 
               frequency=12)
data2.ts

data2.fit <- stl(data2.ts[,3], s.window="periodic")
plot (data2.fit)

#Testing data time series
data3 <-data.te %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5))

data3.ts <- ts(data3, start=c(2016, 3), end=c(2017, 2), 
               frequency=12)
data3.ts

# Seasonal naive method
data.snaive <- snaive(data2.ts[,3], h = 12)
plot(data.snaive)
lines(data2.fit$time.series[,2], col = "red")
lines(data1.ts[,3])

# Evaluate model performance
b_error <- data.snaive$mean - data3.ts[,3]
rmse(b_error)
mae(b_error)

