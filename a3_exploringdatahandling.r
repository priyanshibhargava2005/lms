# Install packages
install.packages("readr")
library(readr)

# Reading CSV file (Graduate Admission)
admission <- read.csv("C:\\Users\\91702\\Desktop\\Admission_Predict.csv")
print(head(admission))

# Reading TXT file (Titanic)
# Assume Titanic dataset stored as titanic.txt with tab separator
titanic <- read.table("C:\\Users\\91702\\Desktop\\Titanic-Dataset.txt", sep = "\t", header = TRUE)
print(head(titanic))

# Writing dataset to a CSV in specific location
write.csv(admission, "C:\\Users\\91702\\Desktop\\Admission_Predict.csv", row.names = FALSE)

# Writing Titanic dataset to a TXT file
write.table(titanic, "C:\\Users\\91702\\Desktop\\Titanic-Dataset.txt", sep = "\t", row.names = FALSE)
