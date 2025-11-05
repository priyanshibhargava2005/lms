# ---------------------------
# Decision Tree Classifier on Breast Cancer Dataset
# ---------------------------

# Step 1: Install and Load Required Packages
install.packages(c("mlbench", "caret", "rpart", "rpart.plot", "ggplot2", "caTools", "e1071"))

library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caTools)
library(e1071)

# ---------------------------
# Step 2: Load and Preprocess Data
# ---------------------------

# Load dataset
data("BreastCancer")
bc <- BreastCancer

# Remove ID column
bc <- bc[, -1]

# Convert factor columns (with numeric values) into numeric type
bc[, 1:9] <- lapply(bc[, 1:9], function(x) as.numeric(as.character(x)))

# Remove rows with missing values
bc <- na.omit(bc)

# Convert target variable 'Class' into factor type (for classification)
bc$Class <- factor(bc$Class, levels = c("benign", "malignant"))

# Check structure
str(bc)

# ---------------------------
# Step 3: Split Data into Training and Testing Sets
# ---------------------------

set.seed(123)
split <- sample.split(bc$Class, SplitRatio = 0.7)
train <- subset(bc, split == TRUE)
test  <- subset(bc, split == FALSE)

# ---------------------------
# Step 4: Train Decision Tree Model
# ---------------------------

model_dt <- rpart(Class ~ ., data = train, method = "class")

# Visualize the tree
rpart.plot(model_dt, main = "Decision Tree for Breast Cancer Classification", type = 2, extra = 106)

# ---------------------------
# Step 5: Make Predictions
# ---------------------------

pred <- predict(model_dt, newdata = test, type = "class")

# ---------------------------
# Step 6: Evaluate Model Performance
# ---------------------------

conf_mat <- confusionMatrix(pred, test$Class)
print(conf_mat)

# Extract key performance metrics properly
accuracy  <- conf_mat$overall["Accuracy"]
precision <- conf_mat$byClass["Pos Pred Value"]  # Correct name for Precision
recall    <- conf_mat$byClass["Sensitivity"]     # Correct name for Recall

# Compute F1-score manually
f1 <- 2 * ((precision * recall) / (precision + recall))

# Display metrics
cat("\nModel Performance Metrics:\n")
cat("Accuracy :", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall   :", round(recall, 4), "\n")
cat("F1-Score :", round(f1, 4), "\n")

# ---------------------------
# Step 7: Data Visualization
# ---------------------------

# Check column names before plotting
print(names(bc))

# 'Cl.thickness' and 'Cell.size' are the correct feature names
ggplot(bc, aes(x = Cl.thickness, y = Cell.size, color = Class)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Decision Regions Based on Two Features",
       x = "Clump Thickness",
       y = "Cell Size") +
  theme_minimal()

