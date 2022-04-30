#CORRELATION MATRIX
Matrix<- data.frame(AllData$PM2.5, AllData$PM10, AllData$SO2 , AllData$NO2, 
                    AllData$CO, AllData$O3, AllData$TEMP, AllData$PRES, AllData$RAIN, AllData$WSPM)
colnames(Matrix) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "RAIN", "WSPM")
pairs.panels(Matrix, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman Correlation Matrix")

corrgram(Matrix, order=FALSE, cor.method = "spearman", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt, main="Spearman correlation")


#COMPARE PM2.5 AND PM10
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


#COMPARE URBAN AND SURBAN SITES
#Remove irrelevant variables and duplicates
data.z <- data.frame(AllData$year, AllData$month, AllData$hour, 
                     AllData$PM2.5, AllData$station)
colnames(data.z) <- c("year", "month", "hour", "PM2.5", "station")
data.z <-na.omit(data.z)

#Urban sites
Aotizhongxin.ts <- ts(data.z[data.z$station == 'Aotizhongxin', ] 
                      %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Aotizhongxin.fit <- stl(Aotizhongxin.ts[,3], s.window="periodic")

Dongsi.ts <- ts(data.z[data.z$station == 'Dongsi', ] 
                %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Dongsi.fit <- stl(Dongsi.ts[,3], s.window="periodic")


Guanyuan.ts <- ts(data.z[data.z$station == 'Guanyuan', ] 
                  %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Guanyuan.fit <- stl(Guanyuan.ts[,3], s.window="periodic")

Gucheng.ts <- ts(data.z[data.z$station == 'Gucheng', ] 
                 %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Gucheng.fit <- stl(Gucheng.ts[,3], s.window="periodic")

Nongzhanguan.ts <- ts(data.z[data.z$station == 'Nongzhanguan', ] 
                      %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Nongzhanguan.fit <- stl(Nongzhanguan.ts[,3], s.window="periodic")


Shunyi.ts <- ts(data.z[data.z$station == 'Shunyi', ] 
                %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Shunyi.fit <- stl(Shunyi.ts[,3], s.window="periodic")

Tiantan.ts <- ts(data.z[data.z$station == 'Tiantan', ] 
                 %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
                start=c(2013, 3), end=c(2017, 2), 
                frequency=12)
Tiantan.fit <- stl(Tiantan.ts[,3], s.window="periodic")

Wanliu.ts <- ts(data.z[data.z$station == 'Wanliu', ] 
                %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
                start=c(2013, 3), end=c(2017, 2), 
                frequency=12)
Wanliu.fit <- stl(Wanliu.ts[,3], s.window="periodic")

Wanshouxigong.ts <- ts(data.z[data.z$station == 'Wanshouxigong', ] 
                       %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
                start=c(2013, 3), end=c(2017, 2), 
                frequency=12)
Wanshouxigong.fit <- stl(Wanshouxigong.ts[,3], s.window="periodic")

#Suburban
Changping.ts <- ts(data.z[data.z$station == 'Changping', ] %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Changping.fit <- stl(Changping.ts[,3], s.window="periodic")

Dingling.ts <- ts(data.z[data.z$station == 'Dingling', ] %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Dingling.fit <- stl(Dingling.ts[,3], s.window="periodic")

Huairou.ts <- ts(data.z[data.z$station == 'Huairou', ] %>% group_by(year,month) %>% summarize(Avg = mean(PM2.5)), 
               start=c(2013, 3), end=c(2017, 2), 
               frequency=12)
Huairou.fit <- stl(Huairou.ts[,3], s.window="periodic")

#Plot urban
plot(Aotizhongxin.fit$time.series[,2], ylim = c(55,105), col = "green", 
     main = "PM2.5 Trends by Site", ylab="Concentration (ug/m^3)", xlab ="Year", lty=2)
lines(Dongsi.fit$time.series[,2], col = "red", lty=2)
lines(Guanyuan.fit$time.series[,2], col = "pink", lty=2)
lines(Gucheng.fit$time.series[,2], col = "black", lty=2)
lines(Nongzhanguan.fit$time.series[,2], col = "yellow", lty=2)
lines(Tiantan.fit$time.series[,2], col = "brown", lty=2)
lines(Wanliu.fit$time.series[,2], col = "purple", lty=2)
lines(Wanshouxigong.fit$time.series[,2], col = "violet", lty=2)

#Plot suburban
lines(Changping.fit$time.series[,2], col = "blue")
lines(Dingling.fit$time.series[,2], col = "cyan")
lines(Huairou.fit$time.series[,2], col = "magenta")
lines(Shunyi.fit$time.series[,2], col = "orange")
