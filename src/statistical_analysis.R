library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(ggfortify)       # Plots of PCA
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
  Precipitationmm = max(Precipitationmm),
  Rain = max(Rain),
  Clear = max(Clear)
  ), by = DateTime]

fit <- lm(
  CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day,
  data = traveldemand)
summary(fit)

pdf("../plots/fit-assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(fit, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(fit, test = 'F')

sqrt(mean(traveldemand$Error^2))
mean(abs(traveldemand$Error / traveldemand$CheckInCount))

traveldemand$Pred <- predict(fit)^4
#traveldemand$PredUpr <- (predict(fit, interval = 'pred')^4)[, 2]
#traveldemand$PredLwr <- (predict(fit, interval = 'pred')^4)[, 3]
traveldemand$Error <- traveldemand$CheckInCount - traveldemand$Pred
#traveldemand$ErrorUpr <- traveldemand$PredLwr - traveldemand$Pred
#traveldemand$ErrorLwr <- traveldemand$PredUpr - traveldemand$Pred
traveldemand$ErrorPct <- traveldemand$Error / traveldemand$Pred

travelcard_week <- traveldemand[('2016-10-03' <= Date) & (Date <= '2016-10-09')]

p <- ggplot(travelcard_week, aes(DateTime, CheckInCount)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = Pred), color = 'red') +
  labs(x = 'Time', y = 'Check In Count') + 
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 13)]$DayOfWeek) +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )

p

tikz(file = "../plots/travelcard_pred.tex", width = 6, height = 2.5, timestamp = FALSE)
print(p)
dev.off()



quantiles <- quantile(traveldemand$ErrorPct, c(.05, .95))
traveldemand$ErrorPctAdj <- traveldemand$ErrorPct
traveldemand[ErrorPct < quantiles[1], ErrorPctAdj := quantiles[1]]
traveldemand[ErrorPct > quantiles[2], ErrorPctAdj := quantiles[2]]

summary(traveldemand$ErrorPctAdj)

data <- merge(
  traveldemand[, list(DateTime, Hour, DayOfWeek, ErrorPctAdj)], 
  weather_hour, 
  by = "DateTime")

ggplot(data, aes(TemperatureC, ErrorPctAdj)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(Clear, ErrorPctAdj, group = Clear)) +
  geom_boxplot(notch = T)


fit_clear <- lm(ErrorPctAdj ~ factor(Clear), data = data)
anova(fit_clear)

ggplot(data, aes(WindSpeedKmH, ErrorPctAdj)) +
  geom_point()

ggplot(data, aes(Rain, ErrorPctAdj, group = Rain)) +
  geom_boxplot()

ggplot(data, aes(GustSpeedKmH, ErrorPctAdj)) +
  geom_point()


data[Rain == 0, AnyRain := 'No']
data[Rain > 0, AnyRain := 'Yes']

ggplot(data, aes(AnyRain, ErrorPctAdj)) + 
  geom_boxplot(notch = TRUE)

fit_anyrain <- lm(ErrorPct ~ AnyRain, data = data)
anova(fit_anyrain)

quantiles_inner <- quantile(data$ErrorPctAdj, c(.25, .75))

data3 <- data[(ErrorPctAdj < quantiles_inner[1]) | (quantiles_inner[2] < ErrorPctAdj)]

data2 <- data3[,!c("DateTime", "Hour", "DayOfWeek", "Clear", "AnyRain", "ErrorPctAdj"), with = FALSE]
data2 <- scale(data2)

pca <- stats::prcomp(data2)

autoplot(pca,
         data = data.frame(ErrorPct = data3[, ErrorPctAdj]),
         colour = 'ErrorPct',
         loadings.label  = TRUE,
         loadings.label.size = 3,
         loadings = TRUE) + 
  scale_colour_gradient2() +
  ggsave('../plots/loadings.pdf', width = 10, height = 5)

summary(data$ErrorPct)

fiterr <- lm(ErrorPctAdj ~ WindSpeedKmH + Rain + TemperatureC + Clear, data = data)

summary(fiterr)

par(mfrow=c(2, 2))
plot(fiterr, which=1:4)

autoplot(cluster::clara(data2, k = 2))

library(broom)
y <-rnorm(10)
x <-1:10
mod <- lm(y ~ x)
df <- augment(mod)

ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()

