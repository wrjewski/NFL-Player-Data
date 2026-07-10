# Debug player stats data structure
library(nflreadr)
library(dplyr)

# Load player stats to see the structure
stats25 <- nflreadr::load_player_stats(seasons = 2025)

cat("Player stats structure:\n")
str(stats25)

cat("\nPlayer stats dimensions:\n")
print(dim(stats25))

cat("\nFirst few rows:\n")
print(head(stats25))

cat("\nColumn names:\n")
print(names(stats25))

cat("\nUnique game_ids in player stats:\n")
if ("game_id" %in% names(stats25)) {
  print(unique(stats25$game_id)[1:10])
} else {
  cat("game_id column not found!\n")
}

# Check what columns contain game information
game_cols <- names(stats25)[grepl("game", names(stats25), ignore.case = TRUE)]
cat("\nColumns containing 'game':\n")
print(game_cols)
