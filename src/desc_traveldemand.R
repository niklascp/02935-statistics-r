library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

source('traveldemand.R')

traveldemand <- prep_traveldemand()

write_example_traveldemand(traveldemand)

# -----------------
# DESCRIPTIVE PLOTS
# -----------------

# Ensure tables export path
dir.create('../plots/', showWarnings = FALSE)

# Line plot by hour of day and ay of week.
p <- ggplot(traveldemand, aes(dow, D, color = dow)) +
  geom_boxplot(notch = TRUE) +
  labs(x = 'Day of week, $\\mathit{dow}$', y = 'Travel demand, $D$') +
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

# Line plot by hour of day and day of week.
traveldemand_week <- traveldemand[, list(D_mean = mean(D)), by = list(dow, tod)]

p <- ggplot(traveldemand_week, aes(as.integer(tod) - .5, D_mean, group = dow, color = dow)) +
  geom_line() +
  labs(x = 'Time of day, $\\mathit{tod}$', y = 'Mean travel demand, $\\mathrm{mean}(\\mathit{D})$', color = 'Day of week, $\\mathit{dow}$:') +
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

