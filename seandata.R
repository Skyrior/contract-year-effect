rm(list=ls())
library(plyr)
library(ggplot2)


data <- read.csv(file = 'nba.csv')

View(data)



ss.pos <- subset(data, select = c('pos'))

table(ss.pos)


reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

position <- ggplot(ss.pos, aes(x=reorder_size(pos))) + geom_bar() + xlab("Position")

position

