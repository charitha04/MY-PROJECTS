#Loading the dataset
data <- read.csv("C:/Users/CHARITHA/Downloads/archive (6)/diabetes.csv")
data


#Loading required libraries
library(ggplot2)
library(dplyr)
library(caTools)
library(caret)
library(pROC)


#Number of columns and rows in the dataset
dim(data)


#Missing values
# Check for missing values in each column
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

#CHCECKING FOR DUPLICATES
duplicates <- data[duplicated(data), ]
duplicates

#DROPPING ABNORMAL DATA
# Define zero counts before and after dropping abnormal data
zero_count_before <- colSums(data == 0)
zero_count_before
data <- data[data$BMI != 0 & data$BloodPressure != 0 & data$Glucose != 0, ]
zero_count_after <- colSums(data == 0)
zero_count_after

#Histograms
numerical_columns <- names(data)[!names(data) %in% "Outcome"]
par(mfrow=c(3, 3), mar=c(4, 4, 4, 4)) # Adjust the margins to make space for the y-label
selected_vars <- c(numerical_columns)
selected_vars <- selected_vars[-length(selected_vars)]
for (col in selected_vars) {
  hist(data[[col]], main=col, xlab=col, ylab='Frequency', col="lightblue", border="black", prob=TRUE)
  mu <- mean(data[[col]])
  sigma <- sd(data[[col]])
  curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, col="red", lwd=2)
}
# Reset plot layout
par(mfrow = c(1, 1))


### OUTLIERS####
# Exclude the OUTCOME
boxplot_data <- data[, -c(9)]
# Create box plots
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))  # Adjust the margin (mar) for spacing
for (col in colnames(boxplot_data)) {
  boxplot(boxplot_data[[col]], main = col, col = "skyblue", border = "darkblue")
}
# Reset the plotting layout
par(mfrow = c(1, 1))


# Dropping outliers for Age, Pregnancies, and BMI
data <- data %>%
  filter(Age >= quantile(Age, 0.05) & Age <= quantile(Age, 0.95),
         Pregnancies >= quantile(Pregnancies, 0.05) & Pregnancies <= quantile(Pregnancies, 0.95),
         BMI >= quantile(BMI, 0.05) & BMI <= quantile(BMI, 0.95))

# Capping outliers for BloodPressure, SkinThickness, Insulin, and DiabetesPedigreeFunction
data <- data %>%
  mutate(
    BloodPressure = pmin(pmax(BloodPressure, quantile(BloodPressure, 0.05)), quantile(BloodPressure, 0.95)),
    SkinThickness = pmin(pmax(SkinThickness, quantile(SkinThickness, 0.05)), quantile(SkinThickness, 0.95)),
    Insulin = pmin(pmax(Insulin, quantile(Insulin, 0.05)), quantile(Insulin, 0.95)),
    DiabetesPedigreeFunction = pmin(pmax(DiabetesPedigreeFunction, quantile(DiabetesPedigreeFunction, 0.05)), quantile(DiabetesPedigreeFunction, 0.95))
  )

# CHECKING FOR OUTLIERS AFTER REMOVING AND REPLACING
boxplot_data <- data[, -c(9)]
# Create box plots
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))  # Adjust the margin (mar) for spacing
for (col in colnames(boxplot_data)) {
  boxplot(boxplot_data[[col]], main = col, col = "skyblue", border = "darkblue")
}
# Reset the plotting layout
par(mfrow = c(1, 1))


###EXPLORATORY DATA ANALYSIS##########

###########IMBALANCED DATA ################


# Assuming your outcome variable is categorical, you can use table() function to get frequencies
outcome_freq <- table(data$Outcome)

# Convert the outcome frequencies to a data frame
outcome_df <- as.data.frame(outcome_freq)

# Rename the columns for better interpretation
names(outcome_df) <- c("Outcome", "Frequency")

# Create the bar plot using ggplot2
bar_plot <- ggplot(outcome_df, aes(x = Outcome, y = Frequency, fill = Outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Outcome Distribution",
       x = "Outcome",
       y = "Frequency") +
  theme_minimal()

# Display the bar plot
print(bar_plot)

#Balancing data

# Separate majority and minority classes
majority_class <- filter(data, Outcome == 0)
minority_class <- filter(data, Outcome == 1)

# Upsample minority class
minority_upsampled <- minority_class[sample(nrow(minority_class), replace = TRUE, size = nrow(majority_class)), ]

# Combine majority class with upsampled minority class
balanced_data <- rbind(majority_class, minority_upsampled)

# Display class counts
table(balanced_data$Outcome)

data<- balanced_data

# Assuming your outcome variable is categorical, you can use table() function to get frequencies
outcome_freq <- table(data$Outcome)

# Convert the outcome frequencies to a data frame
outcome_df <- as.data.frame(outcome_freq)

# Rename the columns for better interpretation
names(outcome_df) <- c("Outcome", "Frequency")

# Create the bar plot using ggplot2
bar_plot <- ggplot(outcome_df, aes(x = Outcome, y = Frequency, fill = Outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Outcome Distribution",
       x = "Outcome",
       y = "Frequency") +
  theme_minimal()

# Display the bar plot
print(bar_plot)





# 1. Scatter plot: Relationship between pregnancies and glucose levels
ggplot(data, aes(x = Pregnancies, y = Glucose, color = factor(Outcome))) +
  geom_point() +
  labs(title = "Relationship between Pregnancies and Glucose Levels",
       x = "Number of Pregnancies",
       y = "Glucose Levels",
       color = "Diabetes Outcome")

# 3. Scatter plot: Relationship between insulin levels and glucose levels
ggplot(data, aes(x = Insulin, y = Glucose, color = factor(Outcome))) +
  geom_point() +
  labs(title = "Relationship between Insulin and Glucose Levels",
       x = "Insulin Levels",
       y = "Glucose Levels",
       color = "Diabetes Outcome")

# 4. Histogram: Distribution of age among diabetic and non-diabetic women
ggplot(data, aes(x = Age, fill = factor(Outcome))) +
  geom_histogram(binwidth = 5, alpha = 0.6) +
  labs(title = "Distribution of Age by Diabetes Outcome",
       x = "Age",
       y = "Frequency",
       fill = "Diabetes Outcome")

# Check for multicollinearity
# Load required packages (if not already done)
# Calculate the correlation matrix
cor_matrix <- cor(data)
# Create a data frame for plotting
cor_df <- as.data.frame(as.table(cor_matrix))
colnames(cor_df) <- c("Variable1", "Variable2", "Correlation")
# Create a heatmap with values
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#1b98e0") +
  theme_minimal() +
  labs(title = "Correlation Heatmap with Values")


#################### MODEL #############################

#DATA PREPARATION
# Define min-max scaling function for features only
min_max_scaling_features <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Apply min-max scaling to features only
scaled_data <- as.data.frame(lapply(data[, -9], min_max_scaling_features))  # Exclude the outcome variable

# Add the outcome variable to the scaled data
scaled_data$Outcome <- data$Outcome


###SPLITING DATA###
# Set seed for reproducibility
set.seed(123)
# Use sample.split to split the data into training and testing sets
split <- sample.split(scaled_data$Outcome, SplitRatio = 0.7)
# Now, you can use the split to create your training and testing sets
train_data <- subset(scaled_data, split == TRUE)
test_data <- subset(scaled_data, split == FALSE)


# Build logistic regression model with all variables
model <- glm(Outcome ~ ., data = train_data, family = binomial)
# Summary of the model
summary(model)
# Make predictions on test data
predictions <- predict(model, newdata = test_data, type = "response")
# Convert probabilities to binary predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)
# Evaluate the model
confusion_matrix <- confusionMatrix(factor(binary_predictions), factor(test_data$Outcome))
confusion_matrix


# Build logistic regression model with significant variables only
significant_model <- glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction + Age, 
                        data = train_data, family = binomial)
# Summary of the significant model
summary(significant_model)
# Make predictions on test data using the significant model
predictions_significant <- predict(significant_model, newdata = test_data, type = "response")
# Convert probabilities to binary predictions
binary_predictions_significant <- ifelse(predictions_significant > 0.5, 1, 0)
# Evaluate the significant model
confusion_matrix_significant <- confusionMatrix(factor(binary_predictions_significant), factor(test_data$Outcome))
confusion_matrix_significant


####ROC CURVES####
# For the initial model
roc_curve_initial <- roc(test_data$Outcome, predictions)
auc_initial <- auc(roc_curve_initial)

# For the significant model
roc_curve_significant <- roc(test_data$Outcome, predictions_significant)
auc_significant <- auc(roc_curve_significant)


# Plot ROC curve for the initial model
plot(roc_curve_initial, col = "blue", main = "ROC Curves for Initial and Significant Models", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Add the ROC curve for the significant model
lines(roc_curve_significant, col = "red")

# Add AUC values for both models
text(0.8, 0.2, paste(round(auc_initial, 2)), col = "blue")
text(0.8, 0.3, paste(round(auc_significant, 2)), col = "red")

# Add a legend with smaller text size (cex)
legend("bottomright", 
       legend = c(paste("Initial Model (AUC =", round(auc_initial, 2), ")"),
                  paste("Significant Model (AUC =", round(auc_significant, 2), ")")),
       col = c("blue", "red"), 
       lwd = 2, 
       cex = 0.8) # Adjust the cex value to change the text size



# Build logistic regression model with significant variables only on training data
significant_model_train <- glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction + Age, 
                               data = train_data, family = binomial)

# Summary of the significant model on training data
summary(significant_model_train)

# Make predictions on training data using the significant model
predictions_train_significant <- predict(significant_model_train, newdata = train_data, type = "response")

# Convert probabilities to binary predictions for training data
binary_predictions_train_significant <- ifelse(predictions_train_significant > 0.5, 1, 0)

# Evaluate the significant model on training data
confusion_matrix_train_significant <- confusionMatrix(factor(binary_predictions_train_significant), factor(train_data$Outcome))
confusion_matrix_train_significant



# Compute the ROC curve
roc_curve_train_significant <- roc(factor(train_data$Outcome), predictions_train_significant)

# Plot the ROC curve
plot(roc_curve_train_significant, main = "ROC Curve - Significant Model (Training Data)",
     col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", lwd = 2)

# Add AUC to the plot
auc_train_significant <- round(auc(roc_curve_train_significant), 2)
text(0.8, 0.3, paste("AUC =", auc_train_significant), col = "blue")

# Add legend
legend("bottomright", legend = paste("AUC =", auc_train_significant), col = "blue", lwd = 2)


# Create a new plot for the significant model
plot(roc_curve_significant, col = "red", main = "ROC Curve - Significant Model(Testing Data)", xlab = "False Positive Rate", ylab = "True Positive Rate")
#text(0.8, 0.3, paste("AUC =", round(auc_significant, 2)), col = "red")

# Add a legend
legend("bottomright", legend = paste("Significant Model (AUC =", round(auc_significant, 2), ")"), col = "red", lwd = 2)

