library(ggplot2)
library(dplyr)
library(reshape)
library(RColorBrewer)



myfunc <- function(name) {
dir <- sprintf("benchmark/%s/rawData.csv", name)

data <- read.csv(dir, header = FALSE, col.names = c('size', 'seed', 'k', 'time'))
data <- subset(data, select=-c(seed))

timeData <- data %>% group_by(size, k) %>% summarise(time = mean(time)) %>% ungroup() %>% arrange(k)

baseData <- timeData %>% filter(k == 0) %>% subset(select=-c(k))
names(baseData) <- c("size", "baseTime")

speedUpData <- timeData %>% filter(k != 0) %>% left_join(baseData, by = "size") %>% transmute(size = size, k = k, speedUp = baseTime / time)

#timeData <- cast(timeData, size~k)
#speedUpData <- cast(speedUpData, size~k)
savePlot <- function(myPlot, fileName) {
  pdf(fileName)
  print(myPlot)
  dev.off()
}
#ggplot(speedUpData, aes(x=size)) + geom_line(aes(y=`8`)) + geom_point(aes(y=`8`))
fig <- ggplot(speedUpData, aes(x = size, y = speedUp, colour = factor(k) )) +
  geom_line() + geom_point(aes(shape=factor(k))) +
  scale_color_brewer(palette="Dark2") +
  scale_shape_manual(values=seq(0,15)) +
  #xlim(22, 26) +
  #scale_y_continuous(breaks = seq(1, 9, by = 0.5)) +
  theme(legend.title=element_blank())
fig
savePlot(fig, sprintf("%s-speedup.pdf", name))

fig2 <- ggplot(timeData, aes(x = size, y = time, colour = factor(k) )) +
  geom_line() +
  scale_color_brewer(palette="Paired") +
  scale_shape_manual(values=seq(0,15)) +
  #xlim(22, 26)+
  theme(legend.title=element_blank())

savePlot(fig2, sprintf("%s-time.pdf", name))
}
myfunc("intcount")
