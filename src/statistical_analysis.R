library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(ggfortify)       # Plots of PCA
library(tikzDevice)      # Beautiful plots in tex/tikz

library(xtable)          # Beautiful tables in tex

source('traveldemand.R')
source('weather.R')

options(xtable.floating = FALSE)
options(xtable.comment = FALSE)
options(xtable.timestamp = "")
options(xtable.hline.after = 0)

ggscreeplot <- function(pcobj, type = c('pev', 'cev')) 
{
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))
  
  yvar.lab <- switch(type,
                     pev = 'Proportion of explained variance',
                     cev = 'Cumulative proportion of explained variance')
  
  df <- data.frame(PC = 1:length(d), yvar = yvar)
  
  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('Principal components') + ylab(yvar.lab) +
    geom_bar(stat = 'identity')
    #geom_point() + geom_path()
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
  Humidity = max(Humidity),
  Rain = max(Rain),
  Clear = max(Clear)
  ), by = DateTime]

#traveldemand <- traveldemand[(DayOfWeek == 'Mon' | DayOfWeek == 'Tue' | DayOfWeek == 'Wed' | DayOfWeek == 'Thu' | DayOfWeek == 'Fri') & (Hour == '7' | Hour == '8' | Hour == '9')]

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

traveldemand$Pred <- predict(fit)^4
traveldemand$Error <- traveldemand$CheckInCount - traveldemand$Pred

eval_initial <- traveldemand[,
                             .(
                               RMSE = sqrt(mean(Error^2)),
                               MAPE = mean(abs(Error / CheckInCount)) * 100
                             ),
                             by = .(DayType, Peek)]
eval_initial_overall <- traveldemand[,
                                     .(
                                       DayType = 'Overall',
                                       Peek = '',
                                       RMSE = sqrt(mean(Error^2)),
                                       MAPE = mean(abs(Error / CheckInCount)) * 100
                                     )]
# Export evaluation table
eval_initial_tab <- rbind(eval_initial, eval_initial_overall, fill = T)
names(eval_initial_tab) <- c('dt', 'peek', 'RMSE', 'MAPE (%)')
print(xtable(eval_initial_tab),
      type = "latex",
      file = "../tables/model_all_eval.tex",
      hline.after = c(0,0:(nrow(eval_initial_tab)-1),nrow(eval_initial_tab)-1),
      include.rownames = FALSE)

# M_np
model_np <- lm(
  CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day,
  data = traveldemand[Peek == 'NO'])
summary(model_np)

pdf("../plots/model_np_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_np, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_wd_np, test = 'F')

traveldemand[Peek == 'NO', Pred := predict(model_np)^4]

# M_mp
model_mp <- lm(
  CheckInCount^(1/4) ~ DayOfWeek + Hour + Day,
  data = traveldemand[Peek == 'MORNING'])
summary(model_mp)

pdf("../plots/model_mp_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_mp, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_mp, test = 'F')

traveldemand[Peek == 'MORNING', Pred := (predict(model_mp)^4)]

# M_ap
model_ap <- lm(
  CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day,
  data = traveldemand[Peek == 'AFTERNOON'])
summary(model_ap)

pdf("../plots/model_ap_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_ap, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_ap, test = 'F')

traveldemand[Peek == 'AFTERNOON', Pred := predict(model_wd_ap)^4]

# Recalculate error
traveldemand$Error <- traveldemand$CheckInCount - traveldemand$Pred
traveldemand$ErrorPct <- traveldemand$Error / traveldemand$Pred

eval_independent <- traveldemand[,
                                 .(
                                   RMSE = sqrt(mean(Error^2)),
                                   MAPE = mean(abs(Error / CheckInCount)) * 100
                                 ),
                                 by = .(DayType, Peek)]
eval_independent_overall <- traveldemand[,
                                     .(
                                       DayType = 'Overall',
                                       Peek = '',
                                       RMSE = sqrt(mean(Error^2)),
                                       MAPE = mean(abs(Error / CheckInCount)) * 100
                                     )]
# Export evaluation table
eval_independent_tab <- rbind(eval_independent, eval_independent_overall, fill = T)
names(eval_independent_tab) <- c('dt', 'peek', 'RMSE', 'MAPE (%)')
print(xtable(eval_independent_tab),
      type = "latex",
      file = "../tables/model_independent_eval.tex",
      hline.after = c(0,0:(nrow(eval_initial_tab)-1),nrow(eval_initial_tab)-1),
      include.rownames = FALSE)

shapiro.test(traveldemand[Peek == 'MORNING']$Error);



#models <- traveldemand[,
#                       .(fit = list(lm(CheckInCount^(1/4) ~ DayOfWeek + Hour + DayOfWeek:Hour + Day, data = .SD))),
#                       by = list(DayType, Peek)]
#setkeyv(models, c('DayType', 'Peek'))

#fill_preds <- function(dt, peek, model) {
#  traveldemand[DayType == dt & Peek == peek, Pred := predict(model)^4]
#}

#models[, fill_preds(DayType, Peek, fit), by = .EACHI]

traveldemand[,
             Pred := predict(models[list(DayType, Peek)]$fit, data = .SD),
             by = list(DayType, Peek)]

lapply(models, function (x) { predict(x) })

# Ensure tables export path
dir.create('../tables/', showWarnings = FALSE)




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
  traveldemand[, list(DateTime, Hour, DayOfWeek, ErrorPct, ErrorPctAdj)], 
  weather_hour, 
  by = "DateTime")

ggplot(data, aes(TemperatureC, ErrorPctAdj)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(factor(Clear, labels = c('No', 'Yes')), ErrorPctAdj, group = Clear)) +
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

fit_anyrain <- lm(ErrorPctAdj ~ AnyRain, data = data)
anova(fit_anyrain)

quantiles_inner <- quantile(data$ErrorPctAdj, c(.25, .75))

data3 <- data[(ErrorPctAdj < quantiles_inner[1]) | (quantiles_inner[2] < ErrorPctAdj)]

data2 <- data3[,!c("DateTime", "Hour", "DayOfWeek", "AnyRain", "ErrorPctAdj"), with = FALSE]
data2 <- scale(data2)

pca <- stats::prcomp(data2)
plot(pca)

pca$rotation

autoplot(pca,
         data = data.frame(ErrorPct = data3[, ErrorPctAdj]),
         colour = 'ErrorPct',
         #frame = TRUE,
         #frame.colour = 'ErrorPct',
         #frame.type = 't',
         loadings.label  = TRUE,
         loadings.label.size = 3,
         loadings = TRUE) + 
  scale_colour_gradient2() +
  ggsave('../plots/loadings.pdf', width = 10, height = 5)

ggscreeplot(pca)



summary(data$ErrorPct)

fiterr <- lm(ErrorPct ~ WindSpeedKmH + Rain + TemperatureC + Clear, data = data)
plot(fiterr)
summary(fiterr)

par(mfrow=c(2, 2))
plot(fiterr, which=1:4)
