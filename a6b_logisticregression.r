#6B)
#Import Iris Dataset from Web Name the dataset and now do Logistic Regression to find out relation between variables that are affecting the accuracy, MAE, MSE on Petal Length, of type of a flower. Also check the model is fit or not.

# LAB-6 B
# Install and load required package
install.packages("Metrics")
library(Metrics)

# Import Iris dataset
iris_data <- read.csv("https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv")

# Convert Species to binary classification (setosa = 1, others = 0)
iris_data$Species_bin <- ifelse(iris_data$species == "setosa", 1, 0)

# Logistic Regression using Petal.Length
log_model <- glm(Species_bin ~ petal_length, data = iris_data, family = "binomial")

# Show model summary
summary(log_model)

# Predictions (probabilities)
pred <- predict(log_model, iris_data, type = "response")

# Model Evaluation
MAE_val <- mae(iris_data$Species_bin, pred)
MSE_val <- mse(iris_data$Species_bin, pred)

cat("MAE:", MAE_val, "\n")
cat("MSE:", MSE_val, "\n")

# Check model fit using Deviance
cat("Null Deviance:", log_model$null.deviance, "\n")
cat("Residual Deviance:", log_model$deviance, "\n")

# Visualization
plot(iris_data$petal_length, iris_data$Species_bin,
     main = "Logistic Regression on Iris Dataset",
     xlab = "Petal Length",
     ylab = "Probability of Setosa")

curve(predict(log_model, data.frame(petal_length = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)
