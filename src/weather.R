library(readr)
library(data.table)

data <- read_csv("../data/EKCH.csv", na = c('N/A', '-', ''), col_types = list(
  TimeCEST = col_skip(), 
  "Wind SpeedKm/h" = col_number()
), locale = )
setDT(data)

rain <- c(
  'Light Rain Showers', 'Light Rain',
  'Rain Showers', 'Rain',
  'Heavy Rain Showers', 'Heavy Rain',
  'Freezing Rain'
)

data$Rain = 0
data[Condition %in% rain]
  


locale()
