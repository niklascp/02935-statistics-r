library(readr)           # Reading data from files
library(data.table)      # Succint and fficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

# -----------------------
# DATA LOAD AND TRANSFORM
# -----------------------

travelcard <- read_delim("../data/travelcard_2.csv", ";")
travelcard$Day <- as.integer(travelcard$Date - min(travelcard$Date))
travelcard$DateTime <- as.POSIXct(format(travelcard$Date)) + travelcard$Hour * 60 * 60
travelcard$DayOfWeek <- factor(travelcard$DayOfWeek, labels = c(tail(locale()$date_names$day_ab, -1), head(locale()$date_names$day_ab, 1)))
travelcard$Hour <- factor(travelcard$Hour)
setDT(travelcard)

travelcard <- travelcard[!(('2016-12-23' <= Date) & (Date <= '2017-01-01'))]

# Create plots dir (if not already exists)
dir.create('../plots/', showWarnings = FALSE)

# -----------------
# DESCRIPTIVE PLOTS
# -----------------

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

tikz(file = "../plots/travelcard_boxplot.tex", width = 6, height = 3)
print(p)
dev.off()

# Line plot by hour of day and ay of week.
travelcard_week <- travelcard[, list(Mean.CheckInCount = mean(CheckInCount), Var = var(CheckInCount)), by = list(DayOfWeek, Hour)]

p <- ggplot(travelcard_week, aes(as.integer(Hour) - .5, Mean.CheckInCount, group = DayOfWeek, color = DayOfWeek)) +
  geom_line() +
  labs(x = 'Hour of day', y = 'Passenger boardings', color = 'Day of week:') +
  scale_x_continuous(breaks = seq(0, 24), labels = head(c(rbind(seq(0, 24, by = 2), rep('', 13))), 25)) +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  theme(
    
  ) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p
tikz(file = "../plots/travelcard_hist.tex", width = 6, height = 3)
print(p)
dev.off()

# --------------------
# STATISTICAL ANALYSIS
# --------------------

fit <- lm(CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day, data = travelcard)
summary(fit)

par(mfrow=c(2, 2))
plot(fit, which=1:4)

drop1(fit, test = 'F')

travelcard$Pred <- predict(fit)^4
travelcard$PredUpr <- (predict(fit, interval = 'conf')^4)[, 2]
travelcard$PredLwr <- (predict(fit, interval = 'conf')^4)[, 3]
travelcard$Error <- travelcard$Pred - travelcard$CheckInCount
travelcard$ErrorUpr <- travelcard$PredUpr - travelcard$Pred
travelcard$ErrorLwr <- travelcard$PredLwr - travelcard$Pred
  
travelcard_week <- travelcard[('2016-10-03' <= Date) & (Date <= '2016-10-09')]

p <- ggplot(travelcard_week, aes(DateTime, CheckInCount)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = Pred), color = 'red') +
  labs(x = 'Time', y = 'Passenger Boardings') + 
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 12)]$DayOfWeek) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1), vjust = 0), 
        axis.text.y = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.8)),
        axis.title.y = element_text( vjust=-4 ),
        axis.title.x = element_text( vjust=-0.5 ))

p

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