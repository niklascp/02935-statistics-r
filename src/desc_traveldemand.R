library(readr)           # Reading data from files
library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

library(xtable)          # Beautiful tables in tex

options(xtable.floating = FALSE)
options(xtable.comment = FALSE)
options(xtable.timestamp = "")
options(xtable.hline.after = 0)

# -----------------------
# DATA LOAD AND TRANSFORM
# -----------------------

travelcard <- as.data.table(read_delim("../data/travelcard_2.csv", ";"))
travelcard <- travelcard[!(('2016-12-23' <= Date) & (Date <= '2017-01-01'))]
travelcard$DayOfWeek <- factor(travelcard$DayOfWeek, 
                               labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
travelcard$Hour <- factor(travelcard$Hour)

# Ensure tables export path
dir.create('../tables/', showWarnings = FALSE)

# Export example table
travelcard_tab <- travelcard
travelcard_tab$Date <- format(travelcard_tab$Date, format = '%Y-%m-%d')
names(travelcard_tab) <- c('Date', 'Hour', 'Date of week', 'Check in count')
print(xtable(head(travelcard_tab, 10)), type="latex", file="../tables/travel_demand_data_example.tex", hline.after = c(0,0:9))

travelcard$Day <- as.integer(travelcard$Date - min(travelcard$Date))
travelcard$DateTime <- as.POSIXct(format(travelcard$Date)) + (as.integer(travelcard$Hour) - 1) * 60 * 60

# -----------------
# DESCRIPTIVE PLOTS
# -----------------

# Ensure tables export path
dir.create('../plots/', showWarnings = FALSE)

# Line plot by hour of day and ay of week.
p <- ggplot(travelcard, aes(DayOfWeek, CheckInCount, color = DayOfWeek)) +
  geom_boxplot(notch = TRUE) +
  labs(x = 'Day of week', y = 'Passenger boardings') +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0)),
    legend.position = "none"
  )
p

tikz(file = "../plots/travelcard_boxplot.tex", width = 6, height = 3, timestamp = FALSE)
print(p)
dev.off()

# Line plot by hour of day and ay of week.
travelcard_week <- travelcard[, list(Mean.CheckInCount = mean(CheckInCount), Var = var(CheckInCount)), by = list(DayOfWeek, Hour)]

p <- ggplot(travelcard_week, aes(as.integer(Hour) - .5, Mean.CheckInCount, group = DayOfWeek, color = DayOfWeek)) +
  geom_line() +
  labs(x = 'Hour of day', y = 'Passenger boardings', color = 'Day of week:') +
  scale_x_continuous(breaks = seq(0, 24), labels = head(c(rbind(seq(0, 24, by = 2), rep('', 13))), 25)) +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0)),
    legend.position = "bottom",
    legend.title = element_text(size = rel(0.8))
  ) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p
tikz(file = "../plots/travelcard_hist.tex", width = 6, height = 3, timestamp = FALSE)
print(p)
dev.off()



# --------------------
# STATISTICAL ANALYSIS
# --------------------
fit_dayofweek <- lm(CheckInCount ~ DayOfWeek, data = travelcard[DayOfWeek %in% c("Mon", "Tue", "Wed", "Thu")])
anova(fit_dayofweek)

fit_dayofweek <- lm(CheckInCount ~ DayOfWeek, data = travelcard[DayOfWeek %in% c("Mon", "Tue", "Wed", "Thu", "Fri")])
anova(fit_dayofweek)

fit <- lm(CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day, data = travelcard)
summary(fit)

par(mfrow=c(2, 2))
plot(fit, which=1:4)

drop1(fit, test = 'F')

travelcard$Pred <- predict(fit)^4
travelcard$PredUpr <- (predict(fit, interval = 'pred')^4)[, 2]
travelcard$PredLwr <- (predict(fit, interval = 'pred')^4)[, 3]
travelcard$Error <- travelcard$CheckInCount - travelcard$Pred
travelcard$ErrorUpr <- travelcard$PredLwr - travelcard$Pred
travelcard$ErrorLwr <- travelcard$PredUpr - travelcard$Pred
  
travelcard_week <- travelcard[('2016-10-03' <= Date) & (Date <= '2016-10-09')]

p <- ggplot(travelcard_week, aes(DateTime, CheckInCount)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = Pred), color = 'red') +
  labs(x = 'Time', y = 'Passenger Boardings') + 
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 12)]$DayOfWeek) +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )

p

tikz(file = "../plots/travelcard_pred.tex", width = 6, height = 3, timestamp = FALSE)
print(p)
dev.off()

p <- ggplot(travelcard_week, aes(DateTime, Error / CheckInCount)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 12)]$DayOfWeek) +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )

p

tikz(file = "../plots/travelcard_error_pct.tex", width = 6, height = 3, timestamp = FALSE)
print(p)
dev.off()


par(mfrow=c(1, 1))
plot(travelcard[, list(RMSE = sqrt(mean(Error^2))), by = Date])
plot(travelcard[, list(MAE = mean(abs(Error))), by = Date])

ggplot(travelcard, aes(DateTime, Error)) +
  geom_point() 
  #geom_ribbon(aes(ymin = ErrorLwr, ymax = ErrorUpr), fill = 'gray30')

travelcard[, list(RMSE = sqrt(mean(Error^2))), by = DayOfWeek, Hour]
travelcard[, list(RMSE = sqrt(mean(Error^2))), by = ]

travelcard[, list(Mean.error = mean(Error)), by = DayOfWeek]
travelcard[, list(Mean.error = mean(Error)), by = Hour]
