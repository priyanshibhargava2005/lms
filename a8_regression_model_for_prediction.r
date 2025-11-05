# ----------------------------------------------------------
# LAB: Regression & Information Theory Analysis
# Dataset: Breast Cancer (Inbuilt from mlbench)
# ----------------------------------------------------------

# Load Required Packages
install.packages("mlbench")
install.packages("caTools")
install.packages("Metrics")
install.packages("infotheo", dependencies = TRUE)
install.packages("modeest")

library(mlbench)
library(caTools)
library(Metrics)
library(infotheo)
library(modeest)

# ----------------------------------------------------------
# Load & Clean Dataset
# ----------------------------------------------------------

data("BreastCancer")
bc <- BreastCancer

# Remove ID column
bc <- bc[, -1]

# Convert feature columns to numeric
bc[, 1:9] <- lapply(bc[, 1:9], function(x) as.numeric(as.character(x)))

# Remove rows with missing values
bc <- na.omit(bc)

# View structure & summary
str(bc)
summary(bc)

# ----------------------------------------------------------
# Descriptive Statistics
# ----------------------------------------------------------

mean_values   <- colMeans(bc[, 1:9])
median_values <- apply(bc[, 1:9], 2, median)
mode_values   <- apply(bc[, 1:9], 2, mfv)
iqr_values    <- apply(bc[, 1:9], 2, IQR)

cat("\n📈 Descriptive Statistics:\n")
cat("\nMean Values:\n"); print(mean_values)
cat("\nMedian Values:\n"); print(median_values)
cat("\nMode Values:\n"); print(mode_values)
cat("\nInterquartile Range (IQR):\n"); print(iqr_values)

# ----------------------------------------------------------
#  Train-Test Split
# ----------------------------------------------------------

set.seed(123)
split <- sample.split(bc$Cell.size, SplitRatio = 0.7)
train <- subset(bc, split == TRUE)
test  <- subset(bc, split == FALSE)

# ----------------------------------------------------------
#  Multiple Linear Regression
# ----------------------------------------------------------

model <- lm(Cell.size ~ Cl.thickness + Cell.shape + Marg.adhesion +
              Epith.c.size + Bare.nuclei + Bl.cromatin +
              Normal.nucleoli + Mitoses, data = train)

# Model Summary
summary(model)

# ----------------------------------------------------------
#  Predictions & Regression Metrics
# ----------------------------------------------------------

pred <- predict(model, newdata = test)

r2        <- summary(model)$r.squared
mae_val   <- mae(test$Cell.size, pred)
mse_val   <- mse(test$Cell.size, pred)
rmse_val  <- rmse(test$Cell.size, pred)

cat("\n Regression Accuracy Metrics:\n")
cat("R-squared:", r2, "\n")
cat("MAE:", mae_val, "\n")
cat("MSE:", mse_val, "\n")
cat("RMSE:", rmse_val, "\n")

# Check model fit quality
if (r2 < 0.4) {
  cat("\n Model is likely Underfitted.\n")
} else if (r2 > 0.9) {
  cat("\n Model may be Overfitted.\n")
} else {
  cat("\n Model is Well-Fitted.\n")
}

# ----------------------------------------------------------
# Binary Classification (Benign vs Malignant)
# ----------------------------------------------------------

actual_class <- ifelse(test$Cell.size >= 5, "malignant", "benign")
pred_class   <- ifelse(pred >= 5, "malignant", "benign")

# Confusion Matrix
conf_matrix <- table(Predicted = pred_class, Actual = actual_class)
print(conf_matrix)

# Classification Metrics
accuracy  <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix["malignant", "malignant"] / sum(conf_matrix["malignant", ])
recall    <- conf_matrix["malignant", "malignant"] / sum(conf_matrix[, "malignant"])

cat("\n Classification Metrics:\n")
cat("Accuracy :", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall   :", recall, "\n")

# ----------------------------------------------------------
# Entropy & Information Gain
# ----------------------------------------------------------

disc_data  <- discretize(bc[, c("Cl.thickness", "Cell.size")])
entropy_X  <- entropy(disc_data$Cl.thickness)
entropy_Y  <- entropy(disc_data$Cell.size)
info_gain  <- mutinformation(disc_data$Cl.thickness, disc_data$Cell.size)

cat("\n📉 Entropy & Information Gain:\n")
cat("Entropy(Cl.thickness):", entropy_X, "\n")
cat("Entropy(Cell.size):", entropy_Y, "\n")
cat("Information Gain (Cl.thickness → Cell.size):", info_gain, "\n")

# ----------------------------------------------------------
# Correlation Analysis
# ----------------------------------------------------------

cat("\n Correlation Matrix:\n")
print(cor(bc[, 1:9]))

# ----------------------------------------------------------
# Visualization
# ----------------------------------------------------------

plot(test$Cell.size, pred,
     main = "Predicted vs Actual (Breast Cancer - Multiple Regression)",
     xlab = "Actual Cell Size",
     ylab = "Predicted Cell Size",
     col = "yellow", pch = 19)
abline(0, 1, col = "red", lwd = 2)
