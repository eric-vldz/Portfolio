# Importing the libraries and dataset
library(ggplot2)
library(dplyr)
library(caTools)
library(Amelia)
adult <- read.csv("adult_sal.csv")

# Check for missing values
print(table(adult$type_employer))

# 1836 Null Values, never worked and without pay.
# Replace with unemployed
unemp <- function(job) {
    job <- as.character(job)
    if (job == "Never-worked" | job == "Without-pay") {
        return("Unemployed")
    } else {
        return(job)
    }
}

# Apply the function and screen values
adult$type_employer <- sapply(adult$type_employer, unemp)
print(table(adult$type_employer))

# Group Local-gov, State-gov, Self-emp-inc, Self-emp-not-inc
group_emp <- function(job) {
    if (job == "Local-gov" | job == "State-gov") {
        return("SL-gov")
    } else if (job == "Self-emp-inc" | job == "Self-emp-not-inc") {
        return("self-emp")
    } else {
        return(job)
    }
}

# Apply the function
adult$type_employer <- sapply(adult$type_employer, group_emp)

# Screen marital status
table(adult$marital)

# Grouping marital status to 3 groups: Married, Not-Married and Never-Married
group_marital <- function(mar) {
    mar <- as.character(mar)

    # Not-Married
    if (mar == "Separated" | mar == "Divorced" | mar == "Widowed") {
        return("Not-Married")

        # Never-Married
    } else if (mar == "Never-married") {
        return(mar)

        # Married
    } else {
        return("Married")
    }
}

# Apply the function
adult$marital <- sapply(adult$marital, group_marital)
print(table(adult$marital))

# Screen country
table(adult$country)

# Grouping countrys
levels(adult$country)

Asia <- c(
    "China", "Hong", "India", "Iran", "Cambodia", "Japan", "Laos",
    "Philippines", "Vietnam", "Taiwan", "Thailand"
)

North.America <- c("Canada", "United-States", "Puerto-Rico")

Europe <- c(
    "England", "France", "Germany", "Greece", "Holand-Netherlands", "Hungary",
    "Ireland", "Italy", "Poland", "Portugal", "Scotland", "Yugoslavia"
)

Latin.and.South.America <- c(
    "Columbia", "Cuba", "Dominican-Republic", "Ecuador",
    "El-Salvador", "Guatemala", "Haiti", "Honduras",
    "Mexico", "Nicaragua", "Outlying-US(Guam-USVI-etc)", "Peru",
    "Jamaica", "Trinadad&Tobago"
)
Other <- c("South")

group_country <- function(ctry) {
    if (ctry %in% Asia) {
        return("Asia")
    } else if (ctry %in% North.America) {
        return("North.America")
    } else if (ctry %in% Europe) {
        return("Europe")
    } else if (ctry %in% Latin.and.South.America) {
        return("Latin.and.South.America")
    } else {
        return("Other")
    }
}

# Apply the function
adult$country <- sapply(adult$country, group_country)

# screen results
table(adult$country)

# assess factors in dataset
str(adult)

# Factorise the dataset
adult$type_employer <- sapply(adult$type_employer, factor)
adult$country <- sapply(adult$country, factor)
adult$marital <- sapply(adult$marital, factor)
adult$income <- sapply(adult, factor)

# look at results again
print(str(adult))

# convert any ?'s to NA
adult[adult == "?"] <- NA

# factorise again
table(adult$type_employer)
adult$type_employer <- sapply(adult$type_employer, factor)
adult$country <- sapply(adult$country, factor)
adult$marital <- sapply(adult$marital, factor)
adult$occupation <- sapply(adult$occupation, factor)

# get missmap
missmap(adult, y.at = c(1), y.labels = c(""), col = c("yellow", "black"))

# Remove observations with missing values
adult <- na.omit(adult)
# validate via missmap
missmap(adult, y.at = c(1), y.labels = c(""), col = c("yellow", "black"))

# Create a histogram of age, with income as the fill color
ggplot(adult, aes(age)) +
    geom_histogram(aes(fill = income), color = "black", binwidth = 1) +
    theme_bw()

# Create a histogram of hours per week, with income as the fill color
ggplot(adult, aes(hr_per_week)) +
    geom_histogram() +
    theme_bw()

# rename the country column to region
names(adult)[names(adult) == "country"] <- "region"

# Create a bar chart of region with income as the fill color
ggplot(adult, aes(region)) +
    geom_bar(aes(fill = income), color = "black") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Build a logistic regression model to classify people into 2 groups, above or below 50k
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70)

# Training Data
train <- subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

# Create a logistic regression model
model = glm(income ~ ., family = binomial(logit), data = train)
