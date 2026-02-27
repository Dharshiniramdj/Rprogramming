x <- data.frame(
  name=c("Anu","Bala","Charan","Divya","Esha"),
  score=c(85,62,90,75,88),
  attempts=c(1,3,2,2,1),
  qualify=c("yes","no","yes","no","yes")
)

x <- rbind(x, data.frame(name="Farhan",score=70,attempts=3,qualify="no"))
x$grade <- c("A","C","A+","B","A","B")

x <- x[order(-x$score),]
write.csv(x,"x.csv",row.names=FALSE)
print(x)

