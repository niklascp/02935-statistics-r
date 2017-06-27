library(readr)           # Reading data from files
library(data.table)      # Succint and efficient manipulation/transformation of data

library(xtable)          # Beautiful tables in tex

# ----------------------------------------
# UTILITY FUNCTIONS FOR TRAVEL DEMAND DATA
# ----------------------------------------

write_example_traveldemand <- function(traveldemand) 
{
  options(xtable.floating = FALSE)
  options(xtable.comment = FALSE)
  options(xtable.timestamp = "")
  options(xtable.hline.after = 0)
  
  # Ensure tables export path
  dir.create('../tables/', showWarnings = FALSE)
  
  # Export example table
  traveldemand_tab <- copy(traveldemand)
  traveldemand_tab$Date <- format(traveldemand_tab$Date, format = '%Y-%m-%d')
  names(traveldemand_tab) <- c('Date', 'Hour', 'Date of week', 'Check in count')
  print(xtable(head(traveldemand_tab, 10)),
        type = "latex",
        file = "../tables/travel_demand_data_example.tex",
        hline.after = c(0,0:9),
        include.rownames = FALSE)
}

# -----------------------
# DATA LOAD AND TRANSFORM
# -----------------------

prep_traveldemand <- function() {
  traveldemand <- as.data.table(read_delim("../data/travelcard_2.csv", ";"))
  traveldemand <- traveldemand[!(('2016-12-23' <= Date) & (Date <= '2017-01-01'))]
  traveldemand$DayOfWeek <- factor(traveldemand$DayOfWeek, 
                                 labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  traveldemand$Hour <- factor(traveldemand$Hour)
  traveldemand
}

transform_traveldemand <- function(traveldemand)
{
  traveldemand$Day <- as.integer(traveldemand$Date - min(traveldemand$Date))
  traveldemand$DateTime <- as.POSIXct(format(traveldemand$Date)) + (as.integer(traveldemand$Hour) - 1) * 60 * 60
  
  traveldemand[, DayType := 'WEEKDAY']
  traveldemand[DayOfWeek == 'Sat', DayType := 'WEEKEND']
  traveldemand[DayOfWeek == 'Sun', DayType := 'WEEKEND']
  traveldemand$DayType = factor(traveldemand$DayType)
  
  traveldemand[, Peek := 'NO']
  traveldemand[DayType == 'WEEKDAY' & Hour %in% c('7', '8'), Peek := 'MORNING']
  traveldemand[DayType == 'WEEKDAY' & Hour %in% c('15', '16', '17'), Peek := 'AFTERNOON']
  traveldemand$Peek = factor(traveldemand$Peek)
  
  traveldemand
}
