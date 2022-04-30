#Correlation Matrix
Matrix<- data.frame(AllData$PM2.5, AllData$PM10, AllData$SO2 , AllData$NO2, 
                    AllData$CO, AllData$O3, AllData$TEMP, AllData$PRES, AllData$RAIN, AllData$WSPM)
colnames(Matrix) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "RAIN", "WSPM")
pairs.panels(Matrix, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman Correlation Matrix")

corrgram(Matrix, order=FALSE, cor.method = "spearman", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt, main="Spearman correlation")

#Compare PM2.5 and PM10
#PM2.5
data.pm2.5 <-data.pm %>% group_by(hour) %>% summarize(Avg = mean(PM2.5))

data.pm2.5.ts <- ts(data.pm2.5, start=c(0), end=c(23), 
               frequency=1)
data.pm2.5.ts

data.pm2.5.fit <- stl(data.pm2.5.ts[,1], s.window="periodic")


#PM10
data.pm10 <-data.pm %>% group_by(hour) %>% summarize(Avg = mean(PM10))

data.pm10.ts <- ts(data.pm10, start=c(0), end=c(23), 
               frequency=1)
data.pm10.ts

data.pm10.fit <- stl(data.pm10.ts[,3], s.window="periodic")

#Plot
plot(data.pm2.5.ts[,2], col = "green", ylim = c(0,140), main = "PM2.5 and PM10", ylab = "Concentration (ug/m^3)")
lines(data.pm10.ts[,2], col = "blue", main = "PM2.5")
lines(data.pm2.5.fit$time.series[,2], col = "purple")
lines(data.pm10.fit$time.series[,2], col = "black")
