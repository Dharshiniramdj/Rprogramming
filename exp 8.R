set.seed(123)
x <- rnorm(100)
y <- table(round(x))
print(as.data.frame(y))

