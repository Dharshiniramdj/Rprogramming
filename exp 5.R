a = c("Short","Medium","Tall","Medium","Tall","Short")
b = factor(a)
print(b)

set.seed(10)
c = sample(LETTERS[1:5], 8, replace=TRUE)
d = factor(c)
print(d)
