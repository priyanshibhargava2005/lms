# Load dataset
data <- read.csv("C:\\Users\\91702\\Desktop\\Admission_Predict.csv")

# Boxplot of GRE scores
boxplot(data$GRE.Score,
        main="Distribution of GRE Scores",
        ylab="GRE Score",
        col="lightblue")

# Scatter plot CGPA vs Chance of Admit
plot(data$CGPA, data$Chance.of.Admit,
     main="Scatter Plot: CGPA vs Chance of Admit",
     xlab="CGPA", ylab="Chance of Admit",
     col="darkgreen", pch=19)

# Outliers in CGPA
boxplot(data$CGPA,
        main="Outliers in CGPA",
        col="orange")

# Print outlier values
outliers <- boxplot.stats(data$CGPA)$out
print(outliers)

# Histogram of TOEFL scores
hist(data$TOEFL.Score,
     main="Histogram of TOEFL Scores",
     xlab="TOEFL Score",
     col="lightgreen",
     breaks=20)

# Bar chart of University Rating
barplot(table(data$University.Rating),
        main="Bar Chart: University Ratings",
        xlab="University Rating",
        ylab="Frequency",
        col=rainbow(length(unique(data$University.Rating))))

# Pie chart of Research (0 = No, 1 = Yes)
pie(table(data$Research),
    main="Pie Chart: Research Experience",
    labels=c("No Research","Research"),
    col=c("red","blue"))
