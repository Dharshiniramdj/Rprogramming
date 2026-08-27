# 1. Interactive Temperature vs Humidity
library(plotly)

p1 <- plot_ly(
  weather,
  x = ~Temperature,
  y = ~Humidity,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 12,
    color = ~Temperature,
    colorscale = "Rainbow"
  ),
  text = ~paste(
    "Date:", Date,
    "<br>Temperature:", Temperature,
    "<br>Humidity:", Humidity
  ),
  hoverinfo = "text"
)

p1 <- p1 %>%
  layout(
    title = "Temperature vs Humidity",
    xaxis = list(title = "Temperature"),
    yaxis = list(title = "Humidity")
  )

p1

# 2. Interactive Temperature vs Rainfall

p2 <- plot_ly(
  weather,
  x = ~Temperature,
  y = ~Rainfall,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 12,
    color = ~Rainfall,
    colorscale = "Blues"
  ),
  text = ~paste(
    "Date:", Date,
    "<br>Temperature:", Temperature,
    "<br>Rainfall:", Rainfall
  ),
  hoverinfo = "text"
)

p2 <- p2 %>%
  layout(
    title = "Temperature vs Rainfall",
    xaxis = list(title = "Temperature"),
    yaxis = list(title = "Rainfall")
  )

p2

# 3. Interactive Temperature Trend

p3 <- plot_ly(
  weather,
  x = ~Date,
  y = ~Temperature,
  type = "scatter",
  mode = "lines+markers",
  line = list(
    color = "red",
    width = 3
  ),
  marker = list(
    size = 9,
    color = "orange"
  ),
  text = ~paste(
    "Date:", Date,
    "<br>Temperature:", Temperature
  ),
  hoverinfo = "text"
)

p3 <- p3 %>%
  layout(
    title = "Daily Temperature Trend",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Temperature")
  )

p3

# 4. Interactive Monthly Weather Parameters

p4 <- plot_ly(
  monthly,
  x = ~Month,
  y = ~Temperature,
  name = "Temperature",
  type = "bar",
  marker = list(color = "orange")
)

p4 <- p4 %>%
  add_trace(
    y = ~Humidity,
    name = "Humidity",
    marker = list(color = "blue")
  ) %>%
  add_trace(
    y = ~Rainfall,
    name = "Rainfall",
    marker = list(color = "green")
  ) %>%
  add_trace(
    y = ~WindSpeed,
    name = "Wind Speed",
    marker = list(color = "purple")
  ) %>%
  layout(
    title = "Monthly Weather Parameters",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Average Value"),
    barmode = "group"
  )

p4
# 5. Interactive Seasonal Temperature

p5 <- plot_ly(
  seasonal,
  x = ~Season,
  y = ~Temperature,
  type = "bar",
  marker = list(
    color = c(
      "orange",
      "red",
      "blue"
    )
  ),
  text = ~Temperature,
  textposition = "auto"
)

p5 <- p5 %>%
  layout(
    title = "Average Temperature by Season",
    xaxis = list(title = "Season"),
    yaxis = list(title = "Temperature")
  )

p5