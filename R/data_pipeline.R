# R/data_pipeline.R
#
# Caches nflreadr pulls to local RDS files so a live Shiny session reads from
# disk instead of re-downloading from nflverse on every session start.
# Populate/refresh the cache out-of-band with etl/refresh_cache.R (cron,
# GitHub Actions, etc.) rather than paying the network cost inside the app.

cache_path <- function(cache_key, cache_dir = "data_cache") {
  file.path(cache_dir, paste0(cache_key, ".rds"))
}

cache_is_fresh <- function(cache_file, max_age_hours) {
  if (!file.exists(cache_file)) return(FALSE)
  age_hours <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours"))
  age_hours < max_age_hours
}

# Get data from cache, or fetch via fetch_fn() and populate the cache if
# stale/missing. fetch_fn is injected so this is testable without network.
get_cached_data <- function(cache_key, fetch_fn, cache_dir = "data_cache",
                             max_age_hours = 12, force_refresh = FALSE) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- cache_path(cache_key, cache_dir)

  if (!force_refresh && cache_is_fresh(cache_file, max_age_hours)) {
    return(readRDS(cache_file))
  }

  data <- fetch_fn()
  saveRDS(data, cache_file)
  data
}

season_key <- function(seasons) paste(range(seasons), collapse = "-")

get_schedules_cached <- function(seasons, cache_dir = "data_cache",
                                   max_age_hours = 12, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("schedules_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_schedules(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_team_stats_cached <- function(seasons, cache_dir = "data_cache",
                                    max_age_hours = 12, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("team_stats_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_team_stats(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_player_stats_cached <- function(seasons, cache_dir = "data_cache",
                                      max_age_hours = 12, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("player_stats_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_player_stats(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_rosters_cached <- function(seasons, cache_dir = "data_cache",
                                 max_age_hours = 12, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("rosters_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_rosters(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_pbp_cached <- function(seasons, cache_dir = "data_cache",
                             max_age_hours = 24, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("pbp_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_pbp(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_snap_counts_cached <- function(seasons, cache_dir = "data_cache",
                                     max_age_hours = 24, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("snap_counts_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_snap_counts(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

get_nextgen_stats_cached <- function(seasons, stat_type = "passing", cache_dir = "data_cache",
                                       max_age_hours = 24, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("nextgen_", stat_type, "_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_nextgen_stats(seasons = seasons, stat_type = stat_type),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}

# Injuries change daily during the season, so this defaults to a much
# shorter cache lifetime than the season-stat fetchers above.
get_injuries_cached <- function(seasons, cache_dir = "data_cache",
                                  max_age_hours = 6, force_refresh = FALSE) {
  get_cached_data(
    cache_key = paste0("injuries_", season_key(seasons)),
    fetch_fn = function() nflreadr::load_injuries(seasons = seasons),
    cache_dir = cache_dir, max_age_hours = max_age_hours, force_refresh = force_refresh
  )
}
