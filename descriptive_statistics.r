getwd()
setwd("C:/Users/joshd/Documents")
getwd()
data(cars)
head(cars)
summary(cars)
summary(mtcars)
summary(iris)
str(cars)
quantile(mtcars$cyl)
mean(mtcars$mpg)
median(mtcars$mpg)
table(mtcars$mpg)
min(mtcars$mpg)
max(mtcars$mpg)
aggregate(mpg ~ cyl, data = mtcars, FUN = mean)

#a. Basic Descriptive Statistics
summary(data)

# Mean, Median, Standard Deviation of GRE, TOEFL, CGPA
mean(data$GRE.Score, na.rm = TRUE)
median(data$TOEFL.Score, na.rm = TRUE)
sd(data$CGPA, na.rm = TRUE)

# b. Subsetting the dataset
# Example: Students with GRE > 320
high_gre <- subset(data, GRE.Score > 320)
print(high_gre)

# Example: Students who did Research (Research = 1)
research_students <- subset(data, Research == 1)
print(research_students)
