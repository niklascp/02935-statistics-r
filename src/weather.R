library(readr)           # Reading data from files
library(data.table)      # Succint and efficient manipulation/transformation of data

library(xtable)          # Beautiful tables in tex

# ----------------------------------
# UTILITY FUNCTIONS FOR WEATHER DATA
# ----------------------------------

rain_levels <- function() 
{
  c(
    'Light Rain Showers', 'Light Rain',
    'Rain Showers', 'Rain',
    'Heavy Rain Showers', 'Heavy Rain',
    'Freezing Rain')
}

write_example_weather <- function(weather)
{
  options(xtable.floating = FALSE)
  options(xtable.comment = FALSE)
  options(xtable.timestamp = "")
  options(xtable.hline.after = 0)
  
  weather_tab <- copy(weather)
  weather_tab[, c("Date") := NULL, with=FALSE]
  weather_tab$DateTime <- format(weather_tab$DateTime, format = '%Y-%m-%d %H:%M')
  weather_tab$Rain <- as.factor(weather_tab$Rain)
  weather_tab$Clear <- as.factor(weather_tab$Clear)
  colnames(weather_tab) <- c("Date/time",
                             "Temperature",
                             "Dew point",
                             "Wind speed",
                             "Humidity",
                             "Precipitation",
                             "Rain",
                             "Clear")
  
  # Ensure tables export path
  dir.create('../tables/', showWarnings = FALSE)
  
  # Export example table
  print(xtable(head(weather_tab, 10)),
        type="latex",
        file="../tables/weather_data_example.tex",
        hline.after = c(0,0:9),
        include.rownames = FALSE)
}

# -------------------------------
# LOAD AND TRANSFORM WEATHER DATA
# -------------------------------

prep_weather <- function()
{
  weather <- read_csv("../data/EKCH.csv", na = c("", "N/A", "-", " "), col_types = list(
    TimeCEST = col_skip(),
    WindSpeedKmH = col_number(),
    GustSpeedKmH = col_skip()
  ))
  setDT(weather)
  
  weather_aggr <- read_csv("../data/EKCH_Aggr.csv", na = c("", "N/A", "-", " "), col_types = list(
    CET = col_date(format = "%Y-%m-%d")
  ))
  setDT(weather_aggr)
  
  # Remove some unlikely measurements
  weather_clean <- weather[(-25 < TemperatureC) & (TemperatureC < 35)]
  
  weather_clean[, .N, by = Conditions]
  
  rain <- rain_levels()
  weather_clean$Rain <- 0
  for (r in 1:length(rain)) {
    weather_clean[Conditions == rain[r]]$Rain = r
  }
  
  weather_clean$Clear <- 0
  weather_clean[Conditions == 'Clear']$Clear <- 1
  
  weather_clean[, c("Events",
                    "Conditions",
                    "SeaLevelPressurehPa",
                    "VisibilityKm",
                    "WindDirection", 
                    "WindDirDegrees",
                    "Precipitationmm") := NULL, with=FALSE]
  
  # Replace Precipitationmm from daily dataset.
  weather_clean <- merge(
    weather_clean, 
    weather_aggr[, list(CET, Precipitationmm)], 
    by = "CET")
  
  colnames(weather_clean)[1] = "Date"
  colnames(weather_clean)[6] = "DateTime"
  weather_clean$DateTime <- as.POSIXct(format(weather_clean$DateTime, tz="Europe/Copenhagen", usetz=TRUE))
  setcolorder(weather_clean, c("Date",
                               "DateTime",
                               "TemperatureC",
                               "DewPointC",
                               "WindSpeedKmH",
                               "Humidity",
                               "Precipitationmm",
                               "Rain",
                               "Clear"))
  weather_clean
}
