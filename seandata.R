rm(list=ls())
library(plyr)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)


data <- read.csv(file = 'nba.csv')

View(data)



ss.pos <- subset(data, select = c('pos'))

table(ss.pos)


reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

position <- ggplot(ss.pos, aes(x=reorder_size(pos))) + geom_bar() + xlab("Position")

position

ss.height <- subset(data, select = c('height'))

table(ss.height)

height <- ggplot(ss.height, aes(x=height)) + geom_bar() + xlab("Height")
height


ss.age <- subset(data, select = c('age'))

table(ss.age)

age <- ggplot(ss.age, aes(x=age)) + geom_bar() + xlab("age")
age

winshares <- ggplot(data = data) + 
  geom_density(aes(x=ws), fill="purple", colour="purple", alpha=0.2) + 
  geom_density(aes(x=ows), fill="red", colour="red", alpha=0.2) +
  geom_density(aes(x=dws), fill="green", colour="green", alpha=0.2) +
  geom_text(data=data,aes(x=ws,label="ws"))

winshares


winshare_48 <- ggplot(data=data) +
  geom_density(aes(x=ws_48), fill="blue", colour="blue", alpha=0.2)

winshare_48

