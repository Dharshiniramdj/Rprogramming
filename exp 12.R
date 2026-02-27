data("airquality")
result <- aggregate(cbind(Ozone,Solar.R,Wind,Temp) ~ Month,
                    data=airquality,
                    FUN=mean,
                    na.rm=TRUE)
print("Monthly average values:")
print(result)