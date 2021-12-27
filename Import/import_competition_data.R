# Script to import data given by the BDB competition
# Tracking and game data obtained from https://www.kaggle.com/c/nfl-big-data-bowl-2021/data
# Additional coverage and targeted receiver data obtained from 
# https://www.kaggle.com/tombliss/additional-data-coverage-schemes-for-week-1/

# Weekly Player Tracking Data 

# Load tracking data
tracking_data <-  paste0("Data/week", 1:17, ".csv")
tracking_data <- lapply(tracking_data, read_csv)

print("Imported Tracking Data")

# Data on games, players, plays
games <- read_csv(paste(MainDir, "/Data/", "games", ".csv", sep = ""))
plays <- read_csv(paste(MainDir, "/Data/", "plays", ".csv", sep = ""))
players <- read_csv(paste(MainDir, "/Data/", "players", ".csv", sep = ""))

print("Imported Game, Play, Players Data")

# Additional Data (Coverage & Targeted Receivers) 
CoveragesWeek1 <- read.csv(paste(MainDir , "/Data/coverages_week1.csv" , sep = ""))
TargetedReceiver <- read.csv(paste(MainDir , "/Data/targetedReceiver.csv" , sep = ""))

print("Imported Coverage, Target Data")

print("Data Importation Complete")





