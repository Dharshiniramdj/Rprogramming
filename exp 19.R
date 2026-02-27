Spend <- c(10,20,30,40,50)
Sales <- c(15,25,35,45,55)

model <- lm(Sales ~ Spend)
predict(model, data.frame(Spend=60))
