# Debug script to check schedule data
library(nflreadr)
library(dplyr)

# Load the same data as the app
sched_all <- nflreadr::load_schedules(seasons = 2025) %>%
  distinct(game_id, .keep_all = TRUE)

# Check the structure and content
cat("Schedule data structure:\n")
str(sched_all)

cat("\nSchedule data dimensions:\n")
print(dim(sched_all))

cat("\nFirst few rows:\n")
print(head(sched_all))

cat("\nWeek column values:\n")
print(unique(sched_all$week))

cat("\nNon-NA weeks:\n")
weeks <- sort(unique(sched_all$week[!is.na(sched_all$week)]))
print(weeks)

cat("\nLength of weeks vector:\n")
print(length(weeks))

# Try loading 2024 data instead
cat("\n\nTrying 2024 data:\n")
sched_2024 <- nflreadr::load_schedules(seasons = 2024) %>%
  distinct(game_id, .keep_all = TRUE)

cat("2024 Schedule data dimensions:\n")
print(dim(sched_2024))

cat("2024 Week column values:\n")
weeks_2024 <- sort(unique(sched_2024$week[!is.na(sched_2024$week)]))
print(weeks_2024)
