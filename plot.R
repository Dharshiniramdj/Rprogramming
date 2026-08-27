library(reshape2)

# Create weather data
weather1 <- data.frame(
  Date = c("2026-01-01", "2026-01-02", "2026-01-03"),
  Temperature = c(28, 30, 29),
  Humidity = c(70, 65, 72),
  Rainfall = c(5, 0, 10),
  WindSpeed = c(12, 15, 10)
)

weather2 <- data.frame(
  Date = c("2026-02-01", "2026-02-02", "2026-02-03"),
  Temperature = c(31, 33, 32),
  Humidity = c(68, 60, 65),
  Rainfall = c(2, 0, 4),
  WindSpeed = c(14, 18, 16)
)

weather3 <- data.frame(
  Date = c("2026-03-01", "2026-03-02", "2026-03-03"),
  Temperature = c(34, NA, 35),
  Humidity = c(60, 62, NA),
  Rainfall = c(0, 8, 3),
  WindSpeed = c(20, 18, 22)
)

# Combine files/data frames
weather <- rbind(
  weather1,
  weather2,
  weather3
)

print(weather)

# Convert Date
weather$Date <- as.Date(weather$Date)

# Add Month
weather$Month <- format(
  weather$Date,
  "%B"
)

# Add Season
weather$Season <- ifelse(
  weather$Month %in% c(
    "December", "January", "February"
  ),
  "Winter",
  ifelse(
    weather$Month %in% c(
      "March", "April", "May"
    ),
    "Summer",
    "Monsoon"
  )
)

# Check missing values
print(colSums(is.na(weather)))

# Display rows with missing values
print(weather[
  !complete.cases(weather),
])

# Replace missing values
weather$Temperature[
  is.na(weather$Temperature)
] <- mean(
  weather$Temperature,
  na.rm = TRUE
)

weather$Humidity[
  is.na(weather$Humidity)
] <- mean(
  weather$Humidity,
  na.rm = TRUE
)

weather$Rainfall[
  is.na(weather$Rainfall)
] <- 0

weather$WindSpeed[
  is.na(weather$WindSpeed)
] <- mean(
  weather$WindSpeed,
  na.rm = TRUE
)

# Statistical summary
print(summary(weather))

# Average weather parameters
cat(
  "Average Temperature:",
  mean(weather$Temperature),
  "\n"
)

cat(
  "Average Humidity:",
  mean(weather$Humidity),
  "\n"
)

cat(
  "Average Rainfall:",
  mean(weather$Rainfall),
  "\n"
)

cat(
  "Average Wind Speed:",
  mean(weather$WindSpeed),
  "\n"
)

# Monthly summary
monthly <- aggregate(
  cbind(
    Temperature,
    Humidity,
    Rainfall,
    WindSpeed
  ) ~ Month,
  data = weather,
  mean
)

print(monthly)

# Seasonal summary
seasonal <- aggregate(
  cbind(
    Temperature,
    Humidity,
    Rainfall,
    WindSpeed
  ) ~ Season,
  data = weather,
  mean
)

print(seasonal)

# Reshape data using melt
long_data <- melt(
  weather,
  id.vars = c(
    "Date",
    "Month",
    "Season"
  ),
  measure.vars = c(
    "Temperature",
    "Humidity",
    "Rainfall",
    "WindSpeed"
  ),
  variable.name = "Parameter",
  value.name = "Value"
)

print(long_data)

# Cast data
wide_data <- dcast(
  long_data,
  Month ~ Parameter,
  fun.aggregate = mean
)

print(wide_data)

# Correlation
correlation <- cor(
  weather[
    c(
      "Temperature",
      "Humidity",
      "Rainfall",
      "WindSpeed"
    )
  ]
)

print(correlation)

# Temperature vs Humidity
plot(
  weather$Temperature,
  weather$Humidity,
  main = "Temperature vs Humidity",
  xlab = "Temperature",
  ylab = "Humidity"
)

# Temperature vs Rainfall
plot(
  weather$Temperature,
  weather$Rainfall,
  main = "Temperature vs Rainfall",
  xlab = "Temperature",
  ylab = "Rainfall"
)

# Temperature trend
plot(
  weather$Date,
  weather$Temperature,
  type = "l",
  main = "Temperature Trend",
  xlab = "Date",
  ylab = "Temperature"
)

# Highest temperature
print(
  weather[
    which.max(weather$Temperature),
  ]
)

# Highest rainfall
print(
  weather[
    which.max(weather$Rainfall),
  ]
)

# Save final data
write.csv(
  weather,
  "normalized_weather.csv",
  row.names = FALSE
)