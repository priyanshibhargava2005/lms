# Install and load required package
install.packages("corrplot")
library(corrplot)

# Use the built-in iris dataset
data(iris)

# 1. Correlation Matrix (numeric features)
iris_numeric <- iris[, 1:4]
cor_matrix <- cor(iris_numeric)
print(cor_matrix)

# 2. Correlation Plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7)

# 3. ANOVA (Analysis of Variance)
ancova_model <- aov(Sepal.Length ~ Species + Sepal.Width, data = iris)
summary(ancova_model)
