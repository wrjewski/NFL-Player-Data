test_that("get_cached_data fetches and writes cache on first call", {
  cache_dir <- tempfile("cache")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  call_count <- 0
  fetch_fn <- function() {
    call_count <<- call_count + 1
    data.frame(x = 1:3)
  }

  result1 <- get_cached_data("mykey", fetch_fn, cache_dir = cache_dir)
  expect_equal(call_count, 1)
  expect_equal(nrow(result1), 3)
  expect_true(file.exists(file.path(cache_dir, "mykey.rds")))

  result2 <- get_cached_data("mykey", fetch_fn, cache_dir = cache_dir)
  expect_equal(call_count, 1) # cache hit, not re-fetched
  expect_equal(result2, result1)
})

test_that("get_cached_data re-fetches when the cache is stale", {
  cache_dir <- tempfile("cache")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  call_count <- 0
  fetch_fn <- function() {
    call_count <<- call_count + 1
    data.frame(x = call_count)
  }

  get_cached_data("k", fetch_fn, cache_dir = cache_dir, max_age_hours = 12)
  expect_equal(call_count, 1)

  cache_file <- file.path(cache_dir, "k.rds")
  Sys.setFileTime(cache_file, Sys.time() - as.difftime(48, units = "hours"))

  result <- get_cached_data("k", fetch_fn, cache_dir = cache_dir, max_age_hours = 12)
  expect_equal(call_count, 2)
  expect_equal(result$x, 2)
})

test_that("get_cached_data honors force_refresh even when cache is fresh", {
  cache_dir <- tempfile("cache")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  call_count <- 0
  fetch_fn <- function() {
    call_count <<- call_count + 1
    call_count
  }

  get_cached_data("k", fetch_fn, cache_dir = cache_dir)
  get_cached_data("k", fetch_fn, cache_dir = cache_dir, force_refresh = TRUE)
  expect_equal(call_count, 2)
})

test_that("season_key produces a stable, readable cache key component", {
  expect_equal(season_key(2020:2024), "2020-2024")
  expect_equal(season_key(2025), "2025-2025")
})

test_that("cached fetchers create distinct cache keys per season range", {
  cache_dir <- tempfile("cache")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  called_with <- c()
  fake_load_schedules <- function(seasons) {
    called_with <<- c(called_with, season_key(seasons))
    data.frame(season = seasons)
  }

  # Exercise get_cached_data directly the same way get_schedules_cached does,
  # to confirm two different season ranges don't collide on one cache file.
  get_cached_data(paste0("schedules_", season_key(2023)),
                   function() fake_load_schedules(2023), cache_dir = cache_dir)
  get_cached_data(paste0("schedules_", season_key(2024)),
                   function() fake_load_schedules(2024), cache_dir = cache_dir)

  expect_equal(length(list.files(cache_dir)), 2)
  expect_setequal(called_with, c("2023-2023", "2024-2024"))
})
