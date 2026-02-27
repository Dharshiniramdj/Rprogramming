data("ChickWeight")
head(ChickWeight)

sorted <- ChickWeight[order(ChickWeight$weight),]
print(sorted)

aggregate(weight ~ Diet, ChickWeight, mean)
