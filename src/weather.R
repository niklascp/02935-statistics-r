library(readr)
library(data.table)

library(ggplot2)
library(plotly)

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


travelcard <- read_delim("C:/Development/02935-statistics-r/data/travelcard_2.csv", ";")
travelcard$DayOfWeek <- factor(travelcard$DayOfWeek)
travelcard$Hour <- factor(travelcard$Hour)
setDT(travelcard)

travelcard_by_day <- travelcard[, list(CheckInCount = sum(CheckInCount)), by = list(Date, DayOfWeek)]

ggplot(travelcard_by_day, aes(Date, CheckInCount)) +
  geom_line() 

travelcard['2017-02-06' <= Date & Date <= '2017-02-12']

travelcard[, list(HourCount = .N), by = list(Date, DayOfWeek)][HourCount != 24]

### FIT

fit <- lm(CheckInCount ~ DayOfWeek + Hour, data = travelcard)
summary(fit)

par(mfrow=c(2, 2))
plot(fit, which=1:4)

travelcard_pred <- travelcard
travelcard_pred$Pred <- predict(fit, newdata = travelcard)
travelcard_pred$Error = travelcard_pred$CheckInCount - travelcard_pred$Pred 

travelcard_pred_by_day <- travelcard_pred[, list(CheckInCount = sum(CheckInCount), Pred = sum(Pred)), by = list(Date)]

mean(travelcard_pred$Error)
mean(abs(travelcard_pred$Error))


pred2 <- df[, list(Pred = sum(Pred)), by = list(Date)]

ggplot() +
  geom_line(data = b2, aes(Date, CheckInCount)) +
  geom_line(data = pred2, aes(Date, Pred), color = 'red') 

b3 <- b2
b3$Diff <- b3$CheckInCount - pred2$Pred 

p1 <- ggplot(b3, aes(Date, Diff)) +
  geom_line() 
  
p2 <- ggplot(weather_aggr, aes(CET, Precipitationmm))  +
  geom_bar(stat = 'identity')

ggplotly(p1)
multiplot(p1, p2, cols=1)
