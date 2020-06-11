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

library(csv)
library(readr)
readr::write_csv(merged_nba, "nba.csv")
