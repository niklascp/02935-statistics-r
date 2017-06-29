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
  traveldemand_tab <- copy(traveldemand[, !c("date"), with=FALSE])
  traveldemand_tab$t <- format(traveldemand_tab$t, format = '%Y-%m-%d %H:%M')
  colnames(traveldemand_tab) <- paste0('$\\mathit{', colnames(traveldemand_tab), '}$')
  
  print(xtable(head(traveldemand_tab, 10)),
        type = "latex",
        file = "../tables/travel_demand_data_example.tex",
        hline.after = c(0,0:9),
        sanitize.text.function=function(x){x},
        include.rownames = TRUE)
}

# -----------------------
# DATA LOAD AND TRANSFORM
# -----------------------

prep_traveldemand <- function() {
  traveldemand <- as.data.table(read_delim("../data/travelcard_2.csv", ";", col_names = c('date', 'tod', 'dow', 'D'), skip = 1))
  # Remove christmas week
  traveldemand <- traveldemand[!(('2016-12-23' <= date) & (date <= '2017-01-01'))]
  # Parse date and reorder
  traveldemand$t <- as.POSIXct(format(traveldemand$date)) + traveldemand$tod * 60 * 60
  setcolorder(traveldemand, c('date', 't', 'tod', 'dow', 'D'))
  traveldemand$dow <- factor(traveldemand$dow, 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  traveldemand$tod <- factor(traveldemand$tod)
  traveldemand
}

transform_traveldemand <- function(traveldemand)
{
  traveldemand$day <- as.integer(traveldemand$date - min(traveldemand$date))
  
  traveldemand[, dt := 'Weekday']
  traveldemand[dow == 'Sat', dt := 'Weekend']
  traveldemand[dow == 'Sun', dt := 'Weekend']
  traveldemand$dt = factor(traveldemand$dt)
  
  traveldemand[, peek := 'No Peek']
  traveldemand[dt == 'Weekday' & tod %in% c('7', '8'), peek := 'Morning']
  traveldemand[dt == 'Weekday' & tod %in% c('15', '16', '17'), peek := 'Afternoon']
  traveldemand$peek = factor(traveldemand$peek)
  
  traveldemand
}
