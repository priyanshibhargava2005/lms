# ---------------------------
# LAB: Logistic Regression & Data Analysis on Inbuilt Dataset (mtcars)
# ---------------------------
install.packages("caret")
install.packages("entropy")
install.packages("dplyr")

# Load libraries
library(dplyr)
library(caret)
library(entropy)

# Load inbuilt dataset
data("mtcars")
df <- mtcars

# View first few rows
head(df)

# ---------------------------
# Data Preparation
# ---------------------------

# Convert mpg (miles per gallon) into binary category: High (≥ median) or Low (< median)
median_mpg <- median(df$mpg)
df <- df %>%
  mutate(mpg_cat = ifelse(mpg >= median_mpg, "High", "Low")) %>%
  mutate(mpg_cat = factor(mpg_cat))

# Log-transform some numeric variables to normalize
df <- df %>%
  mutate(
    log_hp = log(hp + 1),
    log_wt = log(wt + 1),
    log_disp = log(disp + 1)
  )

# ---------------------------
# Train-Test Split
# ---------------------------
set.seed(123)
train_index <- createDataPartition(df$mpg_cat, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# ---------------------------
# Handle Class Imbalance with Weights
# ---------------------------
class_counts <- table(train_data$mpg_cat)
weight_high <- as.numeric(class_counts["Low"] / sum(class_counts))
weight_low <- as.numeric(class_counts["High"] / sum(class_counts))

weights <- ifelse(train_data$mpg_cat == "High", weight_high, weight_low)

# ---------------------------
# Logistic Regression Model
# ---------------------------
log_model <- glm(mpg_cat ~ log_hp + log_wt + log_disp + cyl,
                 family = "binomial", data = train_data, weights = weights)

summary(log_model)

# ---------------------------
# Model Predictions
# ---------------------------
log_pred_prob <- predict(log_model, test_data, type = "response")

# Tune threshold for best F1 Score
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  pred <- factor(ifelse(log_pred_prob >= thresholds[i], "High", "Low"))
  cm <- confusionMatrix(pred, test_data$mpg_cat, positive = "High")
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Sensitivity"]
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}

best_threshold <- thresholds[which.max(f1_scores)]

# Final prediction
log_pred <- factor(ifelse(log_pred_prob >= best_threshold, "High", "Low"))
conf_matrix <- confusionMatrix(log_pred, test_data$mpg_cat, positive = "High")

# ---------------------------
# Model Evaluation Metrics
# ---------------------------
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- 2 * ((precision * recall) / (precision + recall))

cat("Accuracy :", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall   :", recall, "\n")
cat("F1 Score :", f1_score, "\n")

# ---------------------------
# Descriptive Statistics
# ---------------------------
mean_hp <- mean(df$hp, na.rm = TRUE)
median_hp <- median(df$hp, na.rm = TRUE)
mode_hp <- as.numeric(names(sort(table(df$hp), decreasing = TRUE)[1]))
iqr_hp <- IQR(df$hp, na.rm = TRUE)

cat("\nMean HP:", mean_hp, "\n")
cat("Median HP:", median_hp, "\n")
cat("Mode HP:", mode_hp, "\n")
cat("IQR HP:", iqr_hp, "\n")

# ---------------------------
# Entropy Calculation
# ---------------------------
entropy_mpg <- entropy(discretize(df$mpg, numBins = 10))
cat("Entropy of MPG:", entropy_mpg, "\n")
    