library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

source('traveldemand.R')
source('weather.R')

# -----------------
# GET PREPARED DATA
# -----------------

traveldemand <- prep_traveldemand()
traveldemand <- transform_traveldemand(traveldemand)

weather <- prep_weather()

# MOVE ?
weather[,DateTime := as.POSIXct(format(DateTime, '%Y-%m-%d %H:00'))]
weather_hour <- weather[,list(
  TemperatureC = mean(TemperatureC, na.rm = TRUE),
  WindSpeedKmH = max(WindSpeedKmH, na.rm = TRUE),
  GustSpeedKmH = max(GustSpeedKmH),
  Precipitationmm = max(Precipitationmm),
  Rain = max(Rain)
  ), by = DateTime]

fit <- lm(
  CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day,
  data = traveldemand)
summary(fit)

par(mfrow=c(2, 2))
plot(fit, which=1:4)

drop1(fit, test = 'F')

traveldemand$Pred <- predict(fit)^4
traveldemand$PredUpr <- (predict(fit, interval = 'pred')^4)[, 2]
traveldemand$PredLwr <- (predict(fit, interval = 'pred')^4)[, 3]
traveldemand$Error <- traveldemand$CheckInCount - traveldemand$Pred
traveldemand$ErrorUpr <- traveldemand$PredLwr - traveldemand$Pred
traveldemand$ErrorLwr <- traveldemand$PredUpr - traveldemand$Pred
traveldemand$ErrorPct <- traveldemand$Error / traveldemand$Pred

data <- merge(
  traveldemand[, list(DateTime, Hour, DayOfWeek, ErrorPct)], 
  weather_hour, 
  by = "DateTime")
