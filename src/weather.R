library(readr)           # Reading data from files
library(data.table)      # Succint and efficient manipulation/transformation of data

library(xtable)          # Beautiful tables in tex

# ----------------------------------
# UTILITY FUNCTIONS FOR WEATHER DATA
# ----------------------------------

write_example_weather <- function(weather)
{
  options(xtable.floating = FALSE)
  options(xtable.comment = FALSE)
  options(xtable.timestamp = "")
  options(xtable.hline.after = 0)
  
  weather_tab <- copy(weather)
  weather_tab$t <- format(weather_tab$t, format = '%Y-%m-%d %H:%M')
  colnames(weather_tab) <- paste0('$\\mathit{', colnames(weather_tab), '}$')

  # Ensure tables export path
  dir.create('../tables/', showWarnings = FALSE)
  
  # Export example table
  print(xtable(head(weather_tab, 10)),
        type="latex",
        file="../tables/weather_data_example.tex",
        hline.after = c(0,0:9),
        sanitize.text.function=function(x){x},
        include.rownames = TRUE)
}

# -------------------------------
# LOAD AND TRANSFORM WEATHER DATA
# -------------------------------

prep_weather <- function()
{
  weather <- read_csv("../data/EKCH.csv", na = c("", "N/A", "-", " "), col_types = list(
    TimeCEST = col_skip(),
    DewPointC = col_skip(),
    SeaLevelPressurehPa = col_skip(),
    WindSpeedKmH = col_number(),
    GustSpeedKmH = col_skip(),
    Precipitationmm = col_skip(),
    Events = col_skip(),
    WindDirection = col_skip(),
    WindDirDegrees = col_skip(),
    VisibilityKm = col_skip()
  ))
  setDT(weather)
  colnames(weather) <- c("date", "temp", "rh", "ws", "cond", "t") 
  
  weather_aggr <- read_csv("../data/EKCH_Aggr.csv", na = c("", "N/A", "-", " "), col_types = list(
    CET = col_date(format = "%Y-%m-%d")
  ))[c(1,20)]
  setDT(weather_aggr)
  colnames(weather_aggr) <- c("date", "pptn") 
  
  # Remove some unlikely temperature measurements
  weather_clean <- weather[(-25 < temp) & (temp < 35)]
  
  weather_clean[, .N, by = cond][N > 6]
  
  # Consolidate conditions
  weather_clean[cond %in% c('Light Drizzle', 'Light Rain Showers',
                            'Light Rain', 'Light Freezing Drizzle'), condt := 'Light Rain']

  weather_clean[cond %in% c('Drizzle', 'Rain Showers',
                            'Rain', 'Freezing Drizzle',
                            'Freezing Rain'), condt := 'Rain']
  
  weather_clean[cond %in% c('Heavy Drizzle', 'Heavy Rain Showers',
                            'Heavy Rain', 'Heavy Freezing Drizzle'), condt := 'Heavy Rain']
  
  weather_clean[cond %in% c('Light Snow Showers', 'Light Snow',
                            'Snow', 'Heavy Snow'), condt := 'Snow']

  weather_clean[cond %in% c('Clear'), condt := 'Clear']
    
  weather_clean[cond %in% c('Scattered Clouds', 'Partly Cloudy'), condt := 'Cloudy']
  
  weather_clean[cond %in% c('Mostly Cloudy', 'Overcast'), condt := 'Overcast']
    
  weather_clean[, cond := ordered(condt, levels = c('Clear', 'Cloudy', 'Overcast', 'Light Rain', 'Rain', 'Heavy Rain', 'Snow'))][, condt := NULL]

  # Replace Precipitationmm from daily dataset.
  weather_clean <- merge(
    weather_clean, 
    weather_aggr, 
    by = "date")
  
  weather_clean[, date := NULL]
  setcolorder(weather_clean, c("t",
                               "temp",
                               "pptn",
                               "rh",
                               "ws",
                               "cond"))
  
  weather_clean$t <- as.POSIXct(format(weather_clean$t, tz="Europe/Copenhagen", usetz=TRUE))

  weather_clean
}
