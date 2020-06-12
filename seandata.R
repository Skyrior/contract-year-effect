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
  geom_density(aes(x=ws, colour="winshare"), fill="blue", alpha=0.2) + 
  geom_density(aes(x=ows, colour="offensive ws"), fill="red", alpha=0.2) +
  geom_density(aes(x=dws, colour="defensive ws"), fill="yellow", alpha=0.2) +
  scale_colour_manual("type", values = c("yellow","red","blue"))

winshares


winshare_48 <- ggplot(data=data) +
  geom_density(aes(x=ws_48), fill="purple", colour="purple", alpha=0.2)

winshare_48

minutes <- ggplot(data = data) + geom_histogram(aes(x=min, colour = "minutes"),
                                              fill = "green", alpha = 0.4)

minutes
options(scipen=1000000)

salary <- ggplot(data=data) + 
  geom_histogram(aes(x=salary_current, colour = "money"), 
               fill = "grey", alpha = 0.4)

salary

