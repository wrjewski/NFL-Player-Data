# etl/refresh_cache.R
#
# Populates data_cache/ with fresh nflverse data. Run this out-of-band on a
# schedule (cron, GitHub Actions, etc.) instead of pulling live inside the
# Shiny app -- the app just reads whatever is in data_cache/.
#
# Usage: Rscript etl/refresh_cache.R

source("R/data_pipeline.R")

current_season <- lubridate::year(Sys.Date())
history_seasons <- 1999:current_season
recent_seasons <- (current_season - 5):current_season

cat("Refreshing schedules (", min(history_seasons), "-", max(history_seasons), ")...\n")
get_schedules_cached(history_seasons, force_refresh = TRUE)

cat("Refreshing team stats (current season)...\n")
get_team_stats_cached(current_season, force_refresh = TRUE)

cat("Refreshing player stats (", min(history_seasons), "-", max(history_seasons), ")...\n")
get_player_stats_cached(history_seasons, force_refresh = TRUE)

cat("Refreshing rosters (current season)...\n")
get_rosters_cached(current_season, force_refresh = TRUE)

cat("Refreshing snap counts (", min(recent_seasons), "-", max(recent_seasons), ")...\n")
get_snap_counts_cached(recent_seasons, force_refresh = TRUE)

cat("Refreshing Next Gen Stats - passing (", min(recent_seasons), "-", max(recent_seasons), ")...\n")
get_nextgen_stats_cached(recent_seasons, stat_type = "passing", force_refresh = TRUE)

cat("Refreshing injuries (current season)...\n")
get_injuries_cached(current_season, force_refresh = TRUE)

cat("Refreshing play-by-play (", min(recent_seasons), "-", max(recent_seasons), ")... this one is slow.\n")
get_pbp_cached(recent_seasons, force_refresh = TRUE)

cat("Done.\n")
