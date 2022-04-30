# Randomise data and select relevant variables
data <- data.frame(data$No, data$year, data$month, data$hour, 
                        data$PM2.5, data$PM10, data$SO2, data$NO2, data$CO, 
                        data$O3, data$TEMP, data$PRES, data$DEWP, data$RAIN, 
                        data$WSPM)
colnames(data) <- c("No", "year", "month", "hour", "PM2.5", "PM10", "O2", "NO2", "CO", 
                    "O3", "TEMP", "PRES", "DEWP", "RAIN", "WSPM")

data <-na.omit(data)

set.seed(12345)
data.rand <- data[order(runif(383585)), ]

# Compare original and randomised data
summary(data$PM2.5)
summary(data.rand$PM2.5)
head(data$PM2.5)
head(data.rand$PM2.5)


# Split into training (75%) and test (25%) data sets
data.train <- data.rand[1:287688, ]
data.test <- data.rand[287689:383585, ]

str(data.train)
str(data.test)

boxplot(data$PM2.5, data.train$PM2.5, data.test$PM2.5, 
        names = c("all", "train", "test"), col = "Bisque")


# Train a regression tree
set.seed(12345)
m.rpart <- rpart(PM2.5 ~ ., data = data.train)

# Basic tree information
m.rpart

# Detailed tree information
summary(m.rpart)

# Tree diagram
rpart.plot(m.rpart, digits = 3)

# EVALUATE MODEL PERFORMANCE

# generate predictions for the testing dataset
p1.rpart <- predict(m.rpart, data.test)

# compare the distribution of predicted values vs. actual values
summary(p1.rpart)
summary(data.test$PM2.5)

boxplot(data.test$PM2.5, p1.rpart, names = c("Actual", "Predicted"), main = "PM2.5")
plot(data.test$PM2.5, p1.rpart, main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")

# Correlation between actual and predicted
cor.test(data.test$PM2.5, p1.rpart, method = "spearman", exact = FALSE)

# MAE between actual and predicted
MAE(data.test$PM2.5, p1.rpart)

# MAE between actual and mean of actual
MAE(mean(data.test$PM2.5), data.test$PM2.5)

# RMSE between actual and predicted
RMSE(data.test$PM2.5, p1.rpart)

# RMSE between actual and mean of actual
RMSE(mean(data.train$PM2.5), data.test$PM2.5)


# Prune regression tree
m.rpart_prune <- prune(m.rpart, cp = 0.05)
m.rpart_prune
summary(m.rpart_prune)
fancyRpartPlot(m.rpart_prune)

# generate predictions for the testing dataset
p2.rpart <- predict(m.rpart_prune, data.test)
boxplot(data.test$PM2.5, p2.rpart, names = c("Actual", "Predicted"))
plot(data.test$PM2.5, p2.rpart, main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
cor.test(data.test$PM2.5, p2.rpart, method = "spearman", exact = FALSE)

# MAE between actual and predicted
MAE(data.test$PM2.5, p2.rpart)

# RMSE between actual and predicted
RMSE(data.test$PM2.5, p2.rpart)
