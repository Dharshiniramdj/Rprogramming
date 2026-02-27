data("ChickWeight")
m <- lm(weight ~ Time + Diet, data=ChickWeight)
p <- predict(m)
mean((ChickWeight$weight - p)^2)

  