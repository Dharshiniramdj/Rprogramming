############################################################
# R Coding Challenge - Complete Solutions (Problems 1-10)
############################################################


############################################################
# 1. Basic data types and their classes
############################################################
cat("========== Problem 1: Basic Data Types ==========\n")

num_var     <- 25.5          # numeric
int_var     <- 25L           # integer
char_var    <- "Hello R"     # character
logical_var <- TRUE          # logical
complex_var <- 3 + 4i        # complex

cat("numeric  :", num_var,     "-> class:", class(num_var),     "\n")
cat("integer  :", int_var,     "-> class:", class(int_var),     "\n")
cat("character:", char_var,    "-> class:", class(char_var),    "\n")
cat("logical  :", logical_var, "-> class:", class(logical_var), "\n")
cat("complex  :", complex_var, "-> class:", class(complex_var), "\n\n")


############################################################
# 2. List containing a vector, a matrix, and a character string
############################################################
cat("========== Problem 2: List Access ==========\n")

my_vector <- c(10, 20, 30, 40)
my_matrix <- matrix(1:6, nrow = 2, ncol = 3)
my_string <- "This is a character string"

my_list <- list(Vector = my_vector, Matrix = my_matrix, String = my_string)

cat("-- Vector element --\n")
print(my_list$Vector)
cat("-- Matrix element --\n")
print(my_list$Matrix)
cat("-- String element --\n")
print(my_list$String)

# Access individually by index too
cat("\nAccessing by index:\n")
print(my_list[[1]])
print(my_list[[2]])
print(my_list[[3]])
cat("\n")


############################################################
# 3. 4x4 matrix arithmetic: addition, subtraction, multiplication
############################################################
cat("========== Problem 3: Matrix Operations ==========\n")

matA <- matrix(1:16, nrow = 4, ncol = 4)
matB <- matrix(16:1, nrow = 4, ncol = 4)

cat("Matrix A:\n")
print(matA)
cat("Matrix B:\n")
print(matB)

mat_add <- matA + matB
mat_sub <- matA - matB
mat_mul <- matA %*% matB   # true matrix multiplication

cat("A + B:\n")
print(mat_add)
cat("A - B:\n")
print(mat_sub)
cat("A %*% B (matrix multiplication):\n")
print(mat_mul)
cat("\n")


############################################################
# 4. 3D array (2x3x4) with random numbers, extract a slice
############################################################
cat("========== Problem 4: 3D Array Slicing ==========\n")

set.seed(123)  # for reproducibility
my_array <- array(round(runif(2 * 3 * 4, 1, 100)), dim = c(2, 3, 4))

cat("Full 3D array (2 x 3 x 4):\n")
print(my_array)

cat("\nSlice: 2nd 'layer' (matrix along 3rd dimension), array[, , 2]:\n")
print(my_array[, , 2])

cat("\nSlice: 1st row across all layers, array[1, , ]:\n")
print(my_array[1, , ])

cat("\nSingle element, array[2, 3, 4]:", my_array[2, 3, 4], "\n\n")


############################################################
# 5. S3 class "Student" vs S4 class "Student"
############################################################
cat("========== Problem 5: S3 vs S4 Classes ==========\n")

## ---- S3 implementation ----
create_student_s3 <- function(name, age, marks) {
  student <- list(name = name, age = age, marks = marks)
  class(student) <- "Student"
  return(student)
}

# Generic display method for S3
display <- function(x) UseMethod("display")

display.Student <- function(x) {
  cat("[S3] Student Name:", x$name, "| Age:", x$age, "| Marks:", x$marks, "\n")
}

s3_student <- create_student_s3("Alice", 20, 88.5)
display(s3_student)

## ---- S4 implementation ----
setClass("Student", representation(
  name  = "character",
  age   = "numeric",
  marks = "numeric"
))

setGeneric("displayStudent", function(x) standardGeneric("displayStudent"))

setMethod("displayStudent", "Student", function(x) {
  cat("[S4] Student Name:", x@name, "| Age:", x@age, "| Marks:", x@marks, "\n")
})

s4_student <- new("Student", name = "Bob", age = 22, marks = 91.2)
displayStudent(s4_student)

cat("\nComparison:\n")
cat("- S3 uses list + class attribute; fields accessed with $; no formal structure/validation.\n")
cat("- S4 uses setClass with typed 'slots'; fields accessed with @; supports validity checks and formal inheritance.\n\n")


############################################################
# 6. Fibonacci series - for loop and while loop
############################################################
cat("========== Problem 6: Fibonacci Series ==========\n")

n <- 10

## ---- Using for loop ----
cat("Fibonacci (for loop), first", n, "terms:\n")
fib_for <- numeric(n)
fib_for[1] <- 0
if (n > 1) fib_for[2] <- 1
for (i in 3:n) {
  fib_for[i] <- fib_for[i - 1] + fib_for[i - 2]
}
print(fib_for)

## ---- Using while loop ----
cat("Fibonacci (while loop), first", n, "terms:\n")
fib_while <- numeric(n)
fib_while[1] <- 0
fib_while[2] <- 1
i <- 3
while (i <= n) {
  fib_while[i] <- fib_while[i - 1] + fib_while[i - 2]
  i <- i + 1
}
print(fib_while)
cat("\n")


############################################################
# 7. Nested if-else: grade categorization
############################################################
cat("========== Problem 7: Grade Categorization ==========\n")

categorize_grade <- function(marks) {
  if (marks < 0 || marks > 100) {
    cat("Invalid marks entered!\n")
  } else if (marks >= 90) {
    cat("Marks:", marks, "-> Grade: A+ (Outstanding)\n")
  } else if (marks >= 80) {
    cat("Marks:", marks, "-> Grade: A (Excellent)\n")
  } else if (marks >= 70) {
    cat("Marks:", marks, "-> Grade: B (Very Good)\n")
  } else if (marks >= 60) {
    cat("Marks:", marks, "-> Grade: C (Good)\n")
  } else if (marks >= 50) {
    cat("Marks:", marks, "-> Grade: D (Pass)\n")
  } else {
    cat("Marks:", marks, "-> Grade: F (Fail)\n")
  }
}

# Simulated "user input" (replace with: marks <- as.numeric(readLines("stdin", n=1)) for interactive use)
sample_marks <- c(95, 82, 74, 63, 55, 40)
for (m in sample_marks) {
  categorize_grade(m)
}
cat("\n")


############################################################
# 8. Recursive factorial and recursive GCD
############################################################
cat("========== Problem 8: Recursive Factorial & GCD ==========\n")

## ---- Recursive Factorial ----
factorial_recursive <- function(n) {
  if (n < 0) {
    stop("Factorial not defined for negative numbers")
  } else if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * factorial_recursive(n - 1))
  }
}

cat("Factorial of 6:", factorial_recursive(6), "\n")
cat("Factorial of 0:", factorial_recursive(0), "\n")

## ---- Recursive GCD ----
gcd_recursive <- function(a, b) {
  if (b == 0) {
    return(a)
  } else {
    return(gcd_recursive(b, a %% b))
  }
}

cat("GCD of 48 and 18:", gcd_recursive(48, 18), "\n")
cat("GCD of 101 and 10:", gcd_recursive(101, 10), "\n\n")


############################################################
# 9. Function with default arguments: Simple Interest
############################################################
cat("========== Problem 9: Simple Interest with Defaults ==========\n")

simple_interest <- function(principal, rate = 5, time = 1) {
  si <- (principal * rate * time) / 100
  return(si)
}

# Calling with all arguments
cat("SI (P=1000, R=8, T=3):", simple_interest(1000, 8, 3), "\n")

# Calling with only principal (uses default rate=5, time=1)
cat("SI (P=1000, defaults R=5, T=1):", simple_interest(1000), "\n")

# Calling with principal and rate only (uses default time=1)
cat("SI (P=2000, R=10, default T=1):", simple_interest(2000, 10), "\n")

# Calling with named arguments in different order
cat("SI (T=2, P=1500, default R=5):", simple_interest(principal = 1500, time = 2), "\n\n")


############################################################
# 10. Loop over a non-vector set (mixed-type list)
############################################################
cat("========== Problem 10: Loop over Mixed-Type List ==========\n")

mixed_list <- list(42, "Hello", TRUE, 3.14, 5L, c(1, 2, 3), NULL, factor("A"))

for (item in mixed_list) {
  cat("Value:", ifelse(is.null(item), "NULL", paste(item, collapse = ", ")),
      "-> Type:", class(item), "\n")
}