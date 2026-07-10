# Simple debug for player props
library(shiny)
library(dplyr)
library(nflreadr)

# Test the actual data loading
cat("Loading schedule data...\n")
sched_all <- nflreadr::load_schedules(seasons = 2025) %>%
  distinct(game_id, .keep_all = TRUE)

cat("Schedule data loaded. Rows:", nrow(sched_all), "\n")
cat("Sample game IDs:", head(sched_all$game_id, 3), "\n")

# Test player stats loading
cat("\nLoading player stats...\n")
stats25 <- nflreadr::load_player_stats(seasons = 2025)
cat("Player stats loaded. Rows:", nrow(stats25), "\n")
cat("Sample players:", head(unique(stats25$player_name), 3), "\n")

# Test filtering logic
cat("\nTesting filtering logic...\n")
sample_game <- sched_all[1, ]
cat("Sample game:", sample_game$game_id, "\n")
cat("Home team:", sample_game$home_team, "Away team:", sample_game$away_team, "\n")

players <- stats25 %>% 
  filter(week == sample_game$week & 
         (team == sample_game$home_team | team == sample_game$away_team))

cat("Players found for this game:", nrow(players), "\n")
if (nrow(players) > 0) {
  cat("Sample players:", head(players$player_name, 3), "\n")
  cat("Positions:", unique(players$position), "\n")
}
