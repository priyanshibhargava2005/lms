install.packages("caTools")
install.packages("ggplot2")

# Load necessary libraries
library(caTools)
library(ggplot2)

# Load inbuilt dataset
data("mtcars")

# View first few rows
head(mtcars)

# Sampling (70% train, 30% test)
set.seed(123)
split <- sample.split(mtcars$mpg, SplitRatio = 0.7)
train <- subset(mtcars, split == TRUE)
test <- subset(mtcars, split == FALSE)

# Covariance & Correlation between mpg and hp
cov_val <- cov(train$mpg, train$hp)
cor_val <- cor(train$mpg, train$hp)
print(paste("Covariance (Train):", round(cov_val, 3)))
print(paste("Correlation (Train):", round(cor_val, 3)))

# Standard Deviations
sd_train_mpg <- sd(train$mpg)
sd_test_mpg <- sd(test$mpg)
sd_train_hp <- sd(train$hp)
sd_test_hp <- sd(test$hp)

print(paste("Train Std Dev MPG:", round(sd_train_mpg, 3)))
print(paste("Test Std Dev MPG:", round(sd_test_mpg, 3)))
print(paste("Train Std Dev HP:", round(sd_train_hp, 3)))
print(paste("Test Std Dev HP:", round(sd_test_hp, 3)))

# Linear Regression model (predict mpg using hp)
model <- lm(mpg ~ hp, data = train)
summary(model)

# Plot regression line using training data
ggplot() +
  geom_point(data = train, aes(x = hp, y = mpg), color = "blue", alpha = 0.6) +
  geom_point(data = test, aes(x = hp, y = mpg), color = "green", alpha = 0.6) +
  geom_smooth(data = train, aes(x = hp, y = mpg), method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: HP vs MPG\n(Train = Blue, Test = Green)",
       x = "Horsepower (hp)",
       y = "Miles per Gallon (mpg)") +
  theme_minimal()
