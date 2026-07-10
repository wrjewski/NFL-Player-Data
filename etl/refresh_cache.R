# etl/refresh_cache.R
#
# Populates data_cache/ with fresh nflverse data. Run this out-of-band on a
# schedule (cron, GitHub Actions, etc.) instead of pulling live inside the
# Shiny app -- the app just reads whatever is in data_cache/.
#
# Usage: Rscript etl/refresh_cache.R

source("R/data_pipeline.R")

# nflreadr::get_current_season() follows nflverse's own Labor Day cutover
# (not just the calendar year), so this stays correct across the Jan-Aug
# off-season instead of pointing at a not-yet-played season. Rosters use the
# separate mid-March free-agency cutover, since teams build next season's
# roster before that season's games begin.
current_season <- nflreadr::get_current_season()
current_roster_season <- nflreadr::get_current_season(roster = TRUE)
history_seasons <- 1999:current_season
recent_seasons <- (current_season - 5):current_season

cat("Refreshing schedules (", min(history_seasons), "-", max(history_seasons), ")...\n")
get_schedules_cached(history_seasons, force_refresh = TRUE)

cat("Refreshing team stats (current season)...\n")
get_team_stats_cached(current_season, force_refresh = TRUE)

cat("Refreshing player stats (", min(history_seasons), "-", max(history_seasons), ")...\n")
get_player_stats_cached(history_seasons, force_refresh = TRUE)

cat("Refreshing rosters (current stats season)...\n")
get_rosters_cached(current_season, force_refresh = TRUE)

if (current_roster_season != current_season) {
  cat("Refreshing rosters (current roster season)...\n")
  get_rosters_cached(current_roster_season, force_refresh = TRUE)
}

cat("Refreshing snap counts (", min(recent_seasons), "-", max(recent_seasons), ")...\n")
get_snap_counts_cached(recent_seasons, force_refresh = TRUE)

cat("Refreshing Next Gen Stats - passing (", min(recent_seasons), "-", max(recent_seasons), ")...\n")
get_nextgen_stats_cached(recent_seasons, stat_type = "passing", force_refresh = TRUE)

cat("Refreshing injuries (current season)...\n")
get_injuries_cached(current_season, force_refresh = TRUE)

cat("Refreshing play-by-play (", min(recent_seasons), "-", max(recent_seasons), ")... this one is slow.\n")
get_pbp_cached(recent_seasons, force_refresh = TRUE)

cat("Done.\n")
