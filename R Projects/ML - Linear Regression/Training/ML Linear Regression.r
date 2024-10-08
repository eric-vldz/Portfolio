# Importing the dataset, imported ggplot2, ggthemes, dplyr, corrgram, corrplot, caTools in terminal.
df <- read.csv("student-mat.csv", sep = ";")

# Selecting only numeric columns
num.cols <- sapply(df, is.numeric)

# filtering only numeric columns for correlation
cor.data <- cor(df[, num.cols])
print(cor.data)

# Correlation plot
corrplot(cor.data, method = "color")

# Correlation matrix with color and pie charts
corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "Student-mat Correlation Plot")


plot(ggplot(df, aes(x = G3)) +
    geom_histogram(bins = 20, alpha = 0.5, fill = "blue", color = "black"))

# set a seed
set.seed(101)
# split data into train and test
sample <- sample.split(df$G3, SplitRatio = 0.7)
# 70% of data goes to train
train <- subset(df, sample == TRUE)
# 30% of data goes to test
test <- subset(df, sample == FALSE)

# create model, using all variables(~.,)
model <- lm(G3 ~ ., data = train)
# interpret the model
# absences and G2 are statistically significant
print(summary(model))

# residual plot
res <- residuals(model)

# plot residual vs fitted
fitted <- fitted(model)
data <- data.frame(res, fitted)
plot(ggplot(data, aes(x = fitted, y = res)) +
    geom_point() +
    geom_hline(yintercept = 0))
res <- as.data.frame(res)
plot(ggplot(res, aes(res)) +
    geom_histogram(alpha = 0.5, fill = "blue"))

# Predictions
G3.predictions <- predict(model, test)
results <- cbind(G3.predictions, test$G3)
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
sst <- sum((mean(df$G3) - results$Actual)^2)

R2 <- 1 - sse / sst
print(R2)
