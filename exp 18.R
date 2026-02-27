x <- c(10,20,30,40,50)
y <- c(5,15,25,35,45)

par(mfrow=c(2,3))

boxplot(x, main="Boxplot", col="lightblue")
hist(x, main="Histogram", col="lightgreen")
barplot(x, main="Bar Plot", col="orange", names.arg=c("A","B","C","D","E"))
plot(x, type="l", main="Line Chart", col="blue")
plot(x,y, main="Scatter Plot", col="red")
