## Charles Shi, Jonathan Liu, Terry Culpepper, and Sean Choi
## Contract Year Effect in the NBA
##
## -------------------------------------------------------------------------
##
## Data
##
## The raw file can be downloaded at 
## https://github.com/Skyrior/contract-year-effect/blob/master/player_stats.csv
##
## -------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(janitor)
library(stringr)
library(hdm) ## for double lasso
library(ggplot2)
library(stargazer)
library(fastDummies)

## -------------------------------------------------------------------------
##
## Cleaning
##
## Remember to set working directory to source file location.
##
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------
##
## Warning: DO NOT RUN AND COMMIT, MAKE SURE WE ALL HAVE THE SAME
## CLEANED DATASET.
##
## -------------------------------------------------------------------------

data <- read.csv("player_stats.csv", header = TRUE)
contract.year <- read.csv("contractyeardata.csv", header = TRUE)
boxout <- read.csv("boxouts.csv", header = TRUE)
touches <- read.csv("touches.csv", header = TRUE)

## Removes NA

cleaned <- na.omit(data)
cboxout <- na.omit(boxout)
ctouches <- na.omit(touches)
contract.year$Salary.Current <- as.numeric(gsub('[$]', '', contract.year$Salary.Current))
contract.year$Salary.Next <- as.numeric(gsub('[$]', '', contract.year$Salary.Next))

## Note that N/A on next year salary is still valid data! They simply
## do not have a contract signed yet.

contract.year$Salary.Next[is.na(contract.year$Salary.Next)] <- 0
cleaned.contract <- na.omit(contract.year)

## 1. Removes trailing and leading whitespace, 
## 2. Replace inner whitespaces with underscores,
## 3. Replace dots with underscores.
## 4. Removes weird chars.

cleaned <- cleaned %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = str_replace_all(name, "[^[:alnum:] ]", "")) %>%
  mutate(name = str_replace(name, " ", "_"))

cleaned.contract <- cleaned.contract %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

cboxout <- cboxout %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

ctouches <- ctouches %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

## Removes duplicates

cleaned <- unique(cleaned)
cleaned.contract <- unique(cleaned.contract)
cboxout <- unique(cboxout)
ctouches <- unique(ctouches)

## Subsets data with no player floating between teams

cleaned.notot <- cleaned %>%
  filter(team != "TOT")

## -------------------------------------------------------------------------
##
## Merging the Dataset
##
## We proceed to merge the two datasets together.
##
## -------------------------------------------------------------------------

names(cleaned.contract)[names(cleaned.contract) == 'player'] <- 'name'
names(cleaned.contract)[names(cleaned.contract) == 'year'] <- 'season'
names(cboxout)[names(cboxout) == 'player'] <- 'name'
names(cboxout)[names(cboxout) == 'year'] <- 'season'
names(ctouches)[names(ctouches) == 'player'] <- 'name'
names(ctouches)[names(ctouches) == 'year'] <- 'season'
cleaned$season <- as.numeric(str_replace(cleaned$season, "-\\d+", ""))
cleaned$season <- cleaned$season + 1
cleaned$season <- as.factor(cleaned$season)
cleaned.contract$season <- as.factor(cleaned.contract$season)
cboxout$season <- as.factor(cboxout$season)
ctouches$season <- as.factor(ctouches$season)

## Merges contract and advanced stats

boxouts$Season <- boxouts$Season+1
touches$Season <- touches$Season + 1

nbaadvancedstats$Season <- nbaadvancedstats$season

boxouts$name <- boxouts$Player
touches$name <- touches$Player

boxouts$Season <- as.numeric(boxouts$Season)
nbaadvancedstats$Season <- as.numeric(nbaadvancedstats$Season)
touches$Season <- as.numeric(touches$Season)
nbaadvancedstats$name <- gsub("_", " ", nbaadvancedstats$name)

merged_nba <- merge(nbaadvancedstats, boxouts, by=c("name", "Season"))
merged_nba <- merge(merged_nba, touches, by=c("name", "Season"))

nba.advancedstats <- left_join(cleaned, cleaned.contract, by = c("name", "season"))
nba.advancedstats <- na.omit(nba.advancedstats)

write.csv(nba.advancedstats, file = "nbaadvancedstats.csv")

## Merges contract and boxout

nba.boxout <- left_join(cleaned.contract, cboxout, by = c("name", "season"))
nba.boxout <- na.omit(nba.boxout)

write.csv(nba.boxout, file = "nbaboxout.csv")

## Merges contract and touch

nba.touch <- left_join(cleaned.contract, ctouches, by = c("name", "season"))
nba.touch <- na.omit(nba.touch)

write.csv(nba.touch, file = "nbatouches.csv")

## Full merged merges all 4 datasets.

nba.fullmerged <- left_join(cleaned, cleaned.contract, by = c("name", "season"))
nba.fullmerged <- left_join(nba.fullmerged, cboxout, by = c("name", "season"))
nba.fullmerged <- left_join(nba.fullmerged, ctouches, by = c("name", "season"))
nba.fullmerged <- na.omit(nba.fullmerged)

write.csv(nba.all, file = "nbaall.csv")


drop.col <- c("gp", "dist_feet", "dist_miles_off", "dist_miles_def", "avg_speed_off",
          "avg_speed_def", "salary_next")
nba <- nba %>% select(-one_of(drop.col))


write.csv(nba, file = "nba.csv")