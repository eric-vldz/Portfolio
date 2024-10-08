# Install packages
install.packages("corrplot")
install.packages("dplyr")
install.packages("caTools")
install.packages("ggplot2")

# Load packages
library(corrplot)
library(dplyr)
library(caTools)
library(ggplot2)

# Import dataset
bike <- read.csv("bikeshare.csv")

# Create a scatter plot of count vs temp.
ggplot(bike, aes(x = temp, y = count, color = temp)) +
    geom_point(alpha = 0.5)

# convert the datetime column and recreate the scatter plot with color gradient
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike, aes(x = datetime, y = count, color = temp)) +
    geom_point(alpha = 0.5) +
    scale_color_gradient(low = "#51ff00", high = "#ff7b00")

# Create a correlation matrix with the count and temp columns.
cor.ct <- cor(bike[, c("count", "temp")])

# Create a boxplot, y = count, x = factor(season)
ggplot(bike, aes(x = factor(season), y = count)) +
    geom_boxplot(aes(color = factor(season))) +
    theme_bw()

# create an hour column by applying the hour indicted in the date time column as the value
bike$hour <- sapply(bike$datetime, function(x) {
    format(x, "%H")
})

# create a scatter plot of count vs hour, with color gradient based on temp, bike data where workingday = 1.
data1 <- filter(bike, workingday == 1)
ggplot(data1, aes(x = hour, y = count, color = temp)) +
    geom_point(alpha = 0.5, position = position_jitter(w = 1, h = 0)) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"))

# same logic but for workingday = 0
data2 <- filter(bike, workingday == 0)
ggplot(data2, aes(x = hour, y = count, color = temp)) +
    geom_point(alpha = 0.5, position = position_jitter(w = 1, h = 0)) +
    scale_color_gradientn(colors = c("blue", "green", "yellow", "red"))

# Create a linear model
temp.model <- lm(count ~ temp, data = bike)

# test model for rentals at temp = 25, should be the same as 6.0462 + (9.17*25)
temp.test <- data.frame(temp = c(25))
print(predict(temp.model, temp.test))
print(6.0462 + 9.17 * 25)

# change hour to numeric
bike$hour <- as.numeric(bike$hour)

bike.model <- lm(count ~ . - casual - registered - datetime - atemp, data = bike)

print(summary(bike.model))

# See how well you can predict for future data points by creating a train/test split.
# Instead of a random split, your split should be "future" data for test, "previous" data for train.

bike.sorted <- bike[order(bike$datetime), ]

# Find the cutoff point (e.g., last month's data)
last_month <- as.POSIXct("2012-12-01")
split_point <- which(bike$datetime >= last_month)[1] - 1

# Split into train and test sets
train <- bike.sorted[1:split_point, ]
test <- bike.sorted[(split_point + 1):nrow(bike.sorted), ]

# Use the same model as before, but on the training data
bike.model <- lm(count ~ . - casual - registered - datetime - atemp, data = train)

# Use the model to predict on test data
count.predictions <- predict(bike.model, test)

results <- cbind(count.predictions, test$count)
colnames(results) <- c("Predicted", "Actual")
results <- as.data.frame(results)

# Take care of the negative values
to_zero <- function(x) {
    if (x < 0) {
        return(0)
    } else {
        return(x)
    }
}

# Apply the function
results$Predicted <- sapply(results$Predicted, to_zero)

# Calculate the Mean Squared Error
mse <- mean((results$Actual - results$Predicted)^2)

abs <- mean(abs(results$Actual - results$Predicted))

print(mse^.05)

sse <- sum((results$Actual - results$Predicted)^2)
sst <- sum((mean(bike$count) - results$Actual)^2)

R2 <- 1 - sse / sst
print(R2)
