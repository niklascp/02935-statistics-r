library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

source('traveldemand.R')

traveldemand <- prep_traveldemand()

write_example_traveldemand(traveldemand)

traveldemand <- transform_traveldemand(traveldemand)

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

p <- ggplot(travelcard_week, aes(DateTime, Error / Pred)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 12)]$DayOfWeek) +
  scale_y_continuous(breaks = seq(-.75,.75, by = .25)) +
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
