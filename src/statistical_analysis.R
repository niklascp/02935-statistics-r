library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(ggfortify)       # Plots of PCA
library(tikzDevice)      # Beautiful plots in tex/tikz

source('traveldemand.R')
source('weather.R')

ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample=.resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue")
  
  return(p)
}

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

pdf("../plots/fit-assumptions.pdf", width=12, height=6)
par(mfrow=c(2, 2))
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

ggplot(traveldemand, aes(sample=Error))+stat_qq()

ggQQ(fit)

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

