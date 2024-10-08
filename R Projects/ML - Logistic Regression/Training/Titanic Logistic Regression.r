# Logistic Regression with R/Training/Titanic Logistic Regression.r
# Install packages and import dataset
library(ggplot2)
library(Amelia)
library(caTools)
library(dplyr)

df.train <- read.csv("titanic_train.csv")
df.test <- read.csv("titanic_test.csv")

# Check for missing values in train dataset
missmap(df.train, main = "Missing values", col = c("yellow", "black"), legend = FALSE)

# plot count by survived or did not survive
ggplot(df.train, aes(Survived)) +
    geom_bar()

# plot by class
ggplot(df.train, aes(Pclass)) +
    geom_bar(aes(fill = factor(Pclass)))

# plot by gender
ggplot(df.train, aes(Sex)) +
    geom_bar(aes(fill = factor(Sex)))

# plot by age
ggplot(df.train, aes(Age)) +
    geom_histogram(bins = 20, alpha = 0.5, fill = "blue", color = "black")

# plot by sibsp
ggplot(df.train, aes(SibSp)) +
    geom_bar()

# plot by fare
ggplot(df.train, aes(Fare)) +
    geom_histogram(bins = 20, alpha = 0.5, fill = "green", color = "black")

# get boxplot by class vs age
ggplot(df.train, aes(Pclass, Age)) +
    geom_boxplot(aes(fill = factor(Pclass), group = factor(Pclass)), alpha = 0.4) +
    scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) +
    theme_bw()

# For NA values criteria is NA on age by class, applying average age by class. 1st class = 37, 2nd class = 29, 3rd class = 24.
df.trainfixed <- df.train %>%
    mutate(Age = ifelse(!is.na(Age), Age,
        case_when(
            Pclass == 1 ~ 37,
            Pclass == 2 ~ 29,
            Pclass == 3 ~ 24,
        )
    ))

# remove name, ticket, cabin, and embarked
df.trainfixed1 <- subset(df.trainfixed, select = -c(PassengerId, Name, Ticket, Cabin))

# convert survived, Pclas, SibSp, and Parch to factor
df.trainfixed1$Survived <- as.factor(df.trainfixed1$Survived)
df.trainfixed1$Pclass <- as.factor(df.trainfixed1$Pclass)
df.trainfixed1$SibSp <- as.factor(df.trainfixed1$SibSp)
df.trainfixed1$Parch <- as.factor(df.trainfixed1$Parch)


# split data into train and test, 70% of data goes to train
set.seed(101)
split <- sample.split(df.trainfixed1$Survived, SplitRatio = 0.70)
final.train <- subset(df.trainfixed1, split == TRUE)
final.test <- subset(df.trainfixed1, split == FALSE)

# create model to predict survived using all variables(~.,)
final.log.model <- glm(formula = Survived ~ ., family = binomial(link = "logit"), data = final.train)

# test model
fitted.probabilities <- predict(final.log.model, newdata = final.test, type = "response")

# calculate accuracy
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)
misClasificError <- mean(fitted.results != final.test$Survived)

# print accuracy: ~80% vs random guessing at ~50%
print(paste("Accuracy", 1 - misClasificError))
print(table(final.test$Survived, fitted.probabilities > 0.5))
