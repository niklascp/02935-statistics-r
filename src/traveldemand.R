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
  travelcard <- as.data.table(read_delim("../data/travelcard_2.csv", ";"))
  travelcard <- travelcard[!(('2016-12-23' <= Date) & (Date <= '2017-01-01'))]
  travelcard$DayOfWeek <- factor(travelcard$DayOfWeek, 
                                 labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  travelcard$Hour <- factor(travelcard$Hour)
  travelcard
}

transform_traveldemand <- function(traveldemand)
{
  traveldemand$Day <- as.integer(traveldemand$Date - min(traveldemand$Date))
  traveldemand$DateTime <- as.POSIXct(format(traveldemand$Date)) + (as.integer(traveldemand$Hour) - 1) * 60 * 60
  traveldemand
}
