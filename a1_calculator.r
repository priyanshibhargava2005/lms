#a) Using R Console (with and without R objects)
#	Without R objects: 
  10 + 5       # Addition
20 - 8       # Subtraction
4 * 3        # Multiplication
15 / 3       # Division
2 ^ 3        # Exponent (2 to the power 3)

#With R objects: 
a <- 10
b <- 5
sum <- a + b
product <- a * b
sum
product

#b) Using Mathematical Functions

sqrt(25)        # Square root
log(10)         # Natural log
log10(100)      # Log base 10
exp(2)          # Exponential (e^2)
abs(-15)        # Absolute value
sin(pi/2)       # Sine
cos(pi)         # Cosine

#c) Write an R Script for Calculator

# Create R objects
num1 <- 20
num2 <- 10

# Perform operations
addition <- num1 + num2
subtraction <- num1 - num2
multiplication <- num1 * num2
division <- num1 / num2
power <- num1 ^ num2

# Print results
print(paste("Addition:", addition))
print(paste("Subtraction:", subtraction))
print(paste("Multiplication:", multiplication))
print(paste("Division:", division))
print(paste("Power:", power))

# Menu-Driven Calculator in R

repeat {
  cat("\n========== R CALCULATOR ==========\n")
  cat("1. Addition\n")
  cat("2. Subtraction\n")
  cat("3. Multiplication\n")
  cat("4. Division\n")
  cat("5. Power\n")
  cat("6. Exit\n")
  cat("==================================\n")
  
  # Taking user choice
  choice <- as.integer(readline(prompt = "Enter your choice (1-6): "))
  
  # Exit condition
  if (choice == 6) {
    cat("Exiting Calculator... Goodbye!\n")
    break
  }
  
  # Taking two numbers as input
  num1 <- as.numeric(readline(prompt = "Enter first number: "))
  num2 <- as.numeric(readline(prompt = "Enter second number: "))
  
  # Performing operations based on user choice
  if (choice == 1) {
    result <- num1 + num2
    cat("Result:", result, "\n")
  } else if (choice == 2) {
    result <- num1 - num2
    cat("Result:", result, "\n")
  } else if (choice == 3) {
    result <- num1 * num2
    cat("Result:", result, "\n")
  } else if (choice == 4) {
    if (num2 == 0) {
      cat("Error: Division by zero is not allowed!\n")
    } else {
      result <- num1 / num2
      cat("Result:", result, "\n")
    }
  } else if (choice == 5) {
    result <- num1 ^ num2
    cat("Result:", result, "\n")
  } else {
    cat("Invalid choice! Please enter a number between 1 and 6.\n")
  }
}
