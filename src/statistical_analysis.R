library(data.table)      # Succint and efficient manipulation/transformation of data

library(dummies)         # Dummy encoding

library (e1071)          # Misc. functions from Probability Theory Group at TU Wien

library(ggplot2)         # Beautiful plots
library(ggfortify)       # Plots of PCA with ggplot2
library(gridExtra)       # Grid layout for ggplot2
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

ggbiplots <- function(pca, color, n) 
{
  pca_comb <- as.data.table(merge(1:n,1:n,all=TRUE))[x < y]
  setorder(pca_comb, x, y)
  ps <- list()
  for (i in 1:nrow(pca_comb)) {
    p <- autoplot(pca,
                  x = pca_comb[i, ]$x,
                  y = pca_comb[i, ]$y,
                  data = data.frame(Color = color),
                  colour = 'Color',
                  alpha = .5,
                  size = .5,
                  #frame = TRUE,
                  #frame.colour = 'ErrorPct',
                  #frame.type = 't',
                  loadings.label  = TRUE,
                  loadings.label.size = 2,
                  loadings = TRUE) + 
      scale_colour_gradient2(guide = 'none') +
      theme_bw()
    ps[[i]] <- p
  }
  ps
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

# ---------------------------
# TRAVEL DEMAND NORMALIZATION
# ---------------------------

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

traveldemand[Peek == 'AFTERNOON', Pred := predict(model_ap)^4]

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
dir.create('../tables/', showWarnings = FALSE)
eval_independent_tab <- rbind(eval_independent, eval_independent_overall, fill = T)
names(eval_independent_tab) <- c('dt', 'peek', 'RMSE', 'MAPE (%)')
print(xtable(eval_independent_tab),
      type = "latex",
      file = "../tables/model_independent_eval.tex",
      hline.after = c(0,0:(nrow(eval_initial_tab)-1),nrow(eval_initial_tab)-1),
      include.rownames = FALSE)

travelcard_week <- traveldemand[('2016-10-03' <= Date) & (Date <= '2016-10-09')]

p <- ggplot(travelcard_week, aes(DateTime, CheckInCount)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = Pred, color = 'Est. Travel Demand, $\\widehat{D}$'), color = 'red') +
  labs(x = 'Time, $t$', y = 'Travel Demand, $D$') + 
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

p <- ggplot(travelcard_week, aes(DateTime, ErrorPct)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Time, $t$', y = 'Relative error, $re$') + 
  scale_x_datetime(breaks = travelcard_week[(Hour == 12)]$DateTime, labels = travelcard_week[(Hour == 13)]$DayOfWeek) +
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


quantiles <- quantile(traveldemand$ErrorPct, c(.05, .95))

traveldemand[, demand_group := factor(cut(ErrorPct, quantile(traveldemand$ErrorPct, c(0, 1/3, 2/3, 1))), labels = c('Low', 'Normal', 'High'))]

traveldemand$ErrorPctAdj <- traveldemand$ErrorPct
traveldemand[ErrorPct < quantiles[1], ErrorPctAdj := quantiles[1]]
traveldemand[ErrorPct > quantiles[2], ErrorPctAdj := quantiles[2]]
summary(traveldemand$ErrorPctAdj)

data <- merge(
  traveldemand[, list(DateTime, DayType, Peek, ErrorPctAdj, demand_group)], 
  weather_hour, 
  by = "DateTime")

ggplot(data, aes(factor(Clear, labels = c('No', 'Yes')), ErrorPctAdj, group = Clear)) +
  geom_boxplot(notch = T)


fit_clear <- lm(ErrorPctAdj ~ factor(Clear), data = data)
anova(fit_clear)

ggplot(data, aes(WindSpeedKmH, ErrorPctAdj)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(Rain, ErrorPctAdj, group = Rain)) +
  geom_boxplot()

data[Rain == 0, AnyRain := 'No']
data[Rain > 0, AnyRain := 'Yes']

ggplot(data, aes(AnyRain, ErrorPctAdj)) + 
  geom_boxplot(notch = TRUE)

fit_anyrain <- lm(ErrorPctAdj ~ AnyRain, data = data)
anova(fit_anyrain)

# ----------------
# PCA WITH DUMMIES
# ----------------

dummy_dt <- dummy(data$DayType, sep = ':')
dummy_peek <- dummy(data$Peek, sep = ':')
data_dummy <- na.omit(cbind(data[, !c("DateTime", "DayType", "Peek"), with=FALSE], dummy_dt, dummy_peek))

data_dummy_scaled <- scale(data_dummy[, !c("ErrorPctAdj", "demand_group"), with=FALSE])
pca <- stats::prcomp(data_dummy_scaled[, -1])

ps <- ggbiplots(pca, data_dummy$ErrorPctAdj, 5)
pdf(paste0("../plots/pca_loadings.pdf"), width=9, height=12)
do.call("grid.arrange", c(ps, ncol=2))
dev.off()

# ---------------------
# PCA WITH SEGMENTATION
# ---------------------

for (peek in unique(data[, Peek])) {
  data_morning <- na.omit(data[Peek == peek, !c("DateTime", "DayType", "Peek"), with=FALSE])
  data_morning_scaled <- scale(data_morning[, !c('ErrorPctAdj'), with=FALSE])
  pca_morning <- stats::prcomp(data_morning_scaled)
  
  ps <- ggbiplots(pca_morning, data_morning$ErrorPctAdj, 5)
  pdf(paste0("../plots/pca_", tolower(peek), "_loadings.pdf"), width=9, height=12)
  do.call("grid.arrange", c(ps, ncol=2))
  dev.off()
}

# ----------------
# SVM WITH DUMMIES
# ----------------

data_dummy_svm <- data_dummy[, !c("ErrorPctAdj"), with=FALSE]

train_dummy_svm <- data_dummy_svm[1:(.8 * nrow(data_dummy_svm))]
test_dummy_svm <- data_dummy_svm[(.8 * nrow(data_dummy_svm)):nrow(data_dummy_svm)]

model <- svm(demand_group ~ ., train_dummy_svm)
train_res <- predict(model)
test_res <- predict(model, newdata = test_dummy_svm)

test_comp <- data.table(demand_group = test_dummy_svm$demand_group, demand_group_pred = test_res)

#test_comp <- data.table(demand_group = train_dummy_svm$demand_group, demand_group_pred = train_res)

a <- test_comp[, .N, by = list(demand_group_pred, demand_group)]
b <- a[, .(demand_group, freq = N/sum(N)), by = demand_group_pred]

ggplot(b, aes(demand_group_pred, demand_group, fill = freq)) +
  geom_tile() +
  geom_text(aes(label=paste0(format(round(100.0 * freq, 1), format = 'f', nsmall = 1), "%"))) +
  scale_fill_gradient(low = "white", high = "red") + 
  labs(x = Estimated Travel Demand )
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )
  
