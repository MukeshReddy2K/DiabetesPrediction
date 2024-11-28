# Load necessary libraries
library(MASS)  # for the dataset
library(ggplot2)  # for visualization
library(caret)  # for splitting dataset
library(corrplot)  # for correlation visualization
library(dplyr)  # for data manipulation

# Display the libraries used
cat('Libraries used in this project:\n')
cat('- R version', R.version.string, '\n')
cat('- MASS version', packageVersion('MASS'), '\n')
cat('- ggplot2 version', packageVersion('ggplot2'), '\n')
cat('- caret version', packageVersion('caret'), '\n')

# Load the diabetes dataset (the Boston dataset from the MASS package)
data(Boston)
diabetes <- Boston

# Summarize the dataset
cat('Loaded', nrow(diabetes), 'records.\n')

# Get a glimpse of the dataset
str(diabetes)
head(diabetes, 10)

# Examine the distribution of various features using histograms
feature_columns <- colnames(diabetes)

# Plot histograms for each feature
par(mfrow = c(3, 5), mar = c(4, 4, 2, 1))  # Set up plotting space
for (i in 1:length(feature_columns)) {
  hist(diabetes[[feature_columns[i]]], main = feature_columns[i], col = rainbow(20), xlab = feature_columns[i])
}

# View summary statistics for each feature
summary(diabetes)

# Correlation with target (medv in the Boston dataset, equivalent to disease_progression)
correlations <- cor(diabetes)
cat('Correlations with target (medv):\n')
print(correlations[,'medv'])

# Split the data into training and testing sets
set.seed(543)  # Set random seed for reproducibility
trainIndex <- createDataPartition(diabetes$medv, p = 0.8, list = FALSE)
train_data <- diabetes[trainIndex, ]
test_data <- diabetes[-trainIndex, ]

cat('Original set dimensions:', dim(diabetes), '\n')
cat('Training set dimensions:', dim(train_data), '\n')
cat('Test set dimensions:', dim(test_data), '\n')

# Linear regression model
lm_model <- lm(medv ~ ., data = train_data)
cat('Linear regression coefficients:\n')
print(coef(lm_model))

# Compare first ten predictions to actual values
predictions <- predict(lm_model, newdata = test_data)
comparison <- data.frame(Predicted = round(predictions, 2), Actual = test_data$medv)
print(head(comparison, 10))

# Calculate the mean squared error (MSE)
mse <- mean((predictions - test_data$medv)^2)
cat('Mean Squared Error (MSE):', mse, '\n')

# Plot lines of best fit for four features with strongest correlation
strongest_corr_features <- c('crim', 'rm', 'dis', 'tax')  # Example of strong features

# Plot each feature with a regression line
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (feature in strongest_corr_features) {
  plot(test_data[[feature]], predictions, xlab = feature, ylab = 'Predicted Disease Progression',
       main = paste('Line of best fit for', feature), col = 'blue', pch = 19)
  abline(lm(predictions ~ test_data[[feature]]), col = 'red')  # Add line of best fit
}

