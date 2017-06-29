library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

library(xtable)          # Beautiful tables in tex

source('traveldemand.R')

options(xtable.floating = FALSE)
options(xtable.comment = FALSE)
options(xtable.timestamp = "")
options(xtable.hline.after = 0)

write_result_table <- function(traveldemand, file)
{
  # Export evaluation table to latex
  eval <- traveldemand[,
                       .(
                         RMSE = format(round(sqrt(mean(D_error^2)), 2), format = 'f', nsmall = 2),
                         MAPE = paste0(format(round(mean(abs(D_error / D)) * 100, 1), format = 'f', nsmall = 1), '\\%')
                       ),
                       by = .(dt, peek)]
  eval_overall <- traveldemand[,
                               .(
                                 dt = 'Overall',
                                 peek = '',
                                 RMSE = format(round(sqrt(mean(D_error^2)), 2), format = 'f', nsmall = 2),
                                 MAPE = paste0(format(round(mean(abs(D_error / D)) * 100, 1), format = 'f', nsmall = 1), '\\%')
                               )]
  
  # Export evaluation table
  dir.create('../tables/', showWarnings = FALSE)
  eval_independent_tab <- xtable(rbind(eval, eval_overall, fill = T))
  colnames(eval_independent_tab)[1:2] <- paste0('$\\mathit{', colnames(eval_independent_tab)[1:2], '}$')
  align(eval_independent_tab) <- "lllrr"
  print(eval_independent_tab,
        type = "latex",
        file = file,
        hline.after = c(0,0:(nrow(eval_independent_tab)-1),nrow(eval_independent_tab)-1),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)
}

# -----------------
# GET PREPARED DATA
# -----------------

traveldemand <- prep_traveldemand()
traveldemand <- transform_traveldemand(traveldemand)

# ---------------------------
# TRAVEL DEMAND NORMALIZATION
# ---------------------------

model_star <- lm(
  D^(1/4) ~ dow + tod + dow:tod + day,
  data = traveldemand)
summary(model_star)

pdf("../plots/model_star_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_star, which=1:4)
par(mfrow=c(1, 1))
dev.off()

# See that all terms are significant
drop1(model_star, test = 'F')

# Calculate estimated travel demand and error
traveldemand$D_pred <- predict(model_star)^4
traveldemand$D_error <- traveldemand$D - traveldemand$D_pred

# Export evaluation table to latex
write_result_table(traveldemand, "../tables/model_star_eval.tex")

# M_np
model_np <- lm(
  D^(1/4) ~ dow + tod + dow:tod + day,
  data = traveldemand[peek == 'No Peek'])
summary(model_np)

pdf("../plots/model_np_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_np, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_wd_np, test = 'F')

traveldemand[peek == 'No Peek', D_pred := predict(model_np)^4]

# M_mp
model_mp <- lm(
  D^(1/4) ~ dow + tod + day,
  data = traveldemand[peek == 'Morning'])
summary(model_mp)

pdf("../plots/model_mp_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_mp, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_mp, test = 'F')

traveldemand[peek == 'Morning', D_pred := (predict(model_mp)^4)]

# M_ap
model_ap <- lm(
  D^(1/4) ~ dow + tod + dow:tod + day,
  data = traveldemand[peek == 'Afternoon'])
summary(model_ap)

pdf("../plots/model_ap_assumptions.pdf", width=9, height=12)
par(mfrow=c(4, 1))
plot(model_ap, which=1:4)
par(mfrow=c(1, 1))
dev.off()

drop1(model_ap, test = 'F')

traveldemand[peek == 'Afternoon', D_pred := predict(model_ap)^4]

# Recalculate error
traveldemand$D_error <- traveldemand$D - traveldemand$D_pred

# Export evaluation table to latex
write_result_table(traveldemand, "../tables/model_independent_eval.tex")


  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Time, $t$', y = 'Relative error, $re$') + 
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
