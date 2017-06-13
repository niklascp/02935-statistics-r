library(readr)
library(data.table)

library(ggplot2)

weather <- read_csv("../data/EKCH.csv", na = c("", "N/A", "-", " "), col_types = list(
  TimeCEST = col_skip()
))
setDT(weather)

weather_aggr <- read_csv("../data/EKCH_Aggr.csv", na = c("", "N/A", "-", " "), col_types = list(
  CET = col_date(format = "%Y-%m-%d")
))
setDT(weather_aggr)

weather_clean <- weather[(-25 < TemperatureC) & (TemperatureC < 35)]

rain <- c(
  'Light Rain Showers', 'Light Rain',
  'Rain Showers', 'Rain',
  'Heavy Rain Showers', 'Heavy Rain',
  'Freezing Rain'
)

weather_clean$Rain <- 0
for (r in 1:length(rain)) {
  weather_clean[Conditions == rain[r]]$Rain = r
}

weather_clean$DateTime <- as.POSIXct(format(weather_clean$DateUTC, tz="Europe/Copenhagen", usetz=TRUE))
weather_clean$Year <- as.integer(format(weather_clean$DateTime, format = '%Y'))
weather_clean$MonthNo <- as.integer(format(weather_clean$DateTime, format = '%m'))
weather_clean$MonthAbbr <- factor(x = seq(1, 12), labels = locale()$date_names$mon_ab)[weather_clean$MonthNo]

# Replace Precipitationmm from daily dataset.
weather_clean <- merge(
  weather_clean[, !"Precipitationmm", with=FALSE], 
  weather_aggr[, list(CET, Precipitationmm)], 
  by = "CET")

ggplot(weather_aggr, aes(CET)) +
  geom_ribbon(aes(ymin=MinTemperatureC, ymax=MaxTemperatureC), alpha = .25) +
  geom_line(aes(y = MeanTemperatureC), color = 'magenta') 

ggplot(data_clean, aes(DateTime, TemperatureC)) +
  geom_line() 
  geom_bar(aes(DateTime, Rain), stat = 'identity', fill = 'blue')
  #facet_grid(MonthAbbr ~ ., scales = "free_y")


travelcard <- read_delim("C:/Development/02935-statistics-r/data/travelcard.csv", ";")
setDT(travelcard)

b <- travelcard[,list(CheckInCount = sum(CheckInCount)), by = list(Date, DayOfWeek)]
ggplot(b, aes(Date, CheckInCount)) +
  geom_line() 


travelcard_poi <- travelcard[('2017-03-01' < Date) & (Date <= '2017-03-31')]
a <- travelcard_poi[,list(CheckInCount = sum(CheckInCount)), by = list(Date, DayOfWeek)]

ggplot(a, aes(Date, CheckInCount)) +
  geom_line() 
