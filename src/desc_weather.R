library(data.table)      # Succint and efficient manipulation/transformation of data

library(ggplot2)         # Beautiful plots
library(tikzDevice)      # Beautiful plots in tex/tikz

source('weather.R')

# --------------------------
# GET DATA AND WRITE EXAMPLE
# --------------------------

weather <- prep_weather()

write_example_weather(weather)

# ------------------------------
# PLOTS FOR DESCRIPTIVE ANALYSIS
# ------------------------------

rain_freq <- weather[, .N, by = Rain][, Freq := N/sum(N)]

p <- ggplot(rain_freq[Rain > 0], 
            aes(x = factor(Rain, labels = rain_levels()), y = Freq)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Level of rain', y = 'Frequency') +
  theme_bw() +
  theme(
    axis.text.x=element_text(angle = 45, margin=margin(30,0,0,0)),
    axis.title.x=element_text(size = rel(0.8), margin=margin(10,0,0,0)),
    axis.title.y=element_text(size = rel(0.8), margin=margin(0,10,0,0)),
    legend.position = "none"
  )
p  
tikz(file = "../plots/weather_hist.tex", width = 6, height = 3, timestamp = FALSE)
print(p)
dev.off()


#par(mfrow=c(2, 2))
#plot(fit, which=1:4)

#install.packages('signal')
#library(signal)
#p1 <- ggplot(travelcard_pred, aes(DateTime)) +
#  geom_line(aes(y = Error), color = 'red') + 
#  geom_line(aes(y = sgolayfilt(Error, n = 25)), color = 'blue') 
#  
#p2 <- ggplot(weather_aggr, aes(CET, Precipitationmm))  +
# geom_bar(stat = 'identity')

#p1
#ggplotly(p1)
#multiplot(p1, p2, cols=1)
