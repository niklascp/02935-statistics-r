library(data.table)      # Succint and efficient manipulation/transformation of data

library(dummies)         # Dummy encoding

library (e1071)          # Misc. functions from Probability Theory Group at TU Wien

library(ggplot2)         # Beautiful plots
library(ggfortify)       # Plots of PCA with ggplot2
library(gridExtra)       # Grid layout for ggplot2
library(ggcorrplot)
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

# Aggregate weather data
weather[,t := as.POSIXct(format(t, '%Y-%m-%d %H:00'))]
weather_hour <- weather[, .(
  temp = mean(temp, na.rm = TRUE),
  pptn = max(pptn, na.rm = TRUE),
  rh = max(rh, na.rm = TRUE),
  ws = max(ws, na.rm = TRUE),
  cond = ifelse(all(is.na(cond)), max(cond), max(cond, na.rm = TRUE))), by = t]
weather_hour[, cond := ordered(cond, labels = levels(weather$cond))]

# ---------------------------
# TRAVEL DEMAND NORMALIZATION
# ---------------------------

# M_np
model_np <- lm(
  D^(1/4) ~ dow + tod + dow:tod + day,
  data = traveldemand[peek == 'No Peek'])

traveldemand[peek == 'No Peek', D_pred := predict(model_np)^4]

# M_mp
model_mp <- lm(
  D^(1/4) ~ dow + tod + day,
  data = traveldemand[peek == 'Morning'])

traveldemand[peek == 'Morning', D_pred := (predict(model_mp)^4)]

# M_ap
model_ap <- lm(
  D^(1/4) ~ dow + tod + dow:tod + day,
  data = traveldemand[peek == 'Afternoon'])

traveldemand[peek == 'Afternoon', D_pred := predict(model_ap)^4]

# Calculate error and relative error
traveldemand$D_error <- traveldemand$D - traveldemand$D_pred
traveldemand$re <- traveldemand$D_error / traveldemand$D_pred

# -------------------------------
# MERGE TRAVEL DEMAND AND WEATHER
# -------------------------------

quantiles <- quantile(traveldemand$re, c(.05, .95))

traveldemand[, demand_group := factor(cut(re, quantile(traveldemand$re, c(0, 1/3, 2/3, 1))), labels = c('Low', 'Normal', 'High'))]

traveldemand$rea <- traveldemand$re
traveldemand[re < quantiles[1], rea := quantiles[1]]
traveldemand[re > quantiles[2], rea := quantiles[2]]
summary(traveldemand$ErrorPctAdj)

data <- merge(
  traveldemand[, list(t, dt, peek, re, rea, demand_group)], 
  weather_hour, 
  by = "t")

dummy_dt <- dummy(data$dt, sep = ':')
dummy_peek <- dummy(data$peek, sep = ':')
dummy_cond <- dummy(data$cond, sep = ':')
data_numeric <- na.omit(data[, !c("t", "rea", "dt", "peek", "demand_group", "cond"), with=FALSE])
data_dummy <- na.omit(cbind(data[, !c("t", "rea", "dt", "peek", "demand_group", "cond"), with=FALSE], dummy_dt, dummy_peek, dummy_cond))
data_dummy_reduced <- na.omit(cbind(data[, !c("t", "rea", "dt", "peek", "demand_group", "cond"), with=FALSE], dummy_cond))

# -------------------
# UNIVARIATE ANALYSIS
# -------------------

# Correlation of cond
p_cond <- ggplot(data, aes(cond, rea, group = cond)) +
  geom_boxplot(notch = F) +
  labs(x = 'Weather condition, $\\mathit{cond}$', y = 'Relative error, $\\mathit{re}$') +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )

tikz(file = "../plots/cor_cond.tex", width = 6, height = 3, timestamp = FALSE)
print(p_cond)
dev.off()

# Anova of discrete variables
anova(lm(rea ~ cond, data = data))
anova(lm(rea ~ dt, data = data))
anova(lm(rea ~ peek, data = data))

# Correlation matric of continous variables
ggcorrplot(cormat, method = "circle") +
  ggsave("../plots/cor_matrix.pdf", width = 4, height = 4, units = "in")

# Correlation of temp
p_temp <- ggplot(data, aes(temp, rea)) +
  geom_smooth() +
  labs(x = 'Temperature, $\\mathit{temp}$', y = 'Relative error, $\\mathit{re}$') +
  theme_bw() +
  theme(
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0))
  )

p_temp

tikz(file = "../plots/cor_temp.tex", width = 6, height = 3, timestamp = FALSE)
print(p_temp)
dev.off()






#subgroup 
#ggplot(data, aes(AnyRain, ErrorPctAdj)) + 
#  geom_boxplot(notch = TRUE)

ggplot(data, aes(ws, rea)) +
  #geom_point() +
  geom_smooth()

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
  
