# ---------------------------------------------
# Logistic Regression: Admission Prediction
# ---------------------------------------------

# Load required packages
if(!require(foreign)) install.packages("foreign", dependencies=TRUE)
if(!require(MASS)) install.packages("MASS", dependencies=TRUE)
if(!require(pscl)) install.packages("pscl", dependencies=TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)

install.packages("pscl")

library(foreign)
library(MASS)
library(pscl)
library(ggplot2)

# Import data
admission_data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# Convert rank to factor
admission_data$rank <- factor(admission_data$rank)

# Logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = admission_data, family = "binomial")

# Summary
summary(model)

# Model fit (McFadden R²)
pR2(model)

# Predictions
predicted_prob <- predict(model, type = "response")
admission_data$predicted <- ifelse(predicted_prob > 0.5, 1, 0)

# Confusion matrix + accuracy
cat("\nConfusion Matrix:\n")
print(table(Actual = admission_data$admit, Predicted = admission_data$predicted))
cat("\nModel Accuracy:", round(mean(admission_data$admit == admission_data$predicted) * 100, 2), "%\n")

# Visualization: GPA vs Predicted Probability
ggplot(admission_data, aes(x = gpa, y = predicted_prob, color = rank)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Logistic Regression: Probability of Admission",
       x = "GPA", y = "Predicted Probability of Admission") +
  theme_minimal()
