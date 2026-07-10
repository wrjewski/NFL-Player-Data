test_that("equal-strength teams converge to ratings close to the raw average", {
  team_stats <- data.frame(
    team = c("A", "B", "C", "D", "A", "B", "C", "D"),
    opponent_team = c("B", "A", "D", "C", "C", "D", "A", "B"),
    season = 2024,
    passing_yards = 200,
    rushing_yards = 100
  )
  ratings <- compute_opponent_adjusted_ratings(team_stats)

  expect_equal(nrow(ratings), 4)
  expect_true(all(abs(ratings$adj_offense - 300) < 1e-6))
  expect_true(all(abs(ratings$adj_defense - 300) < 1e-6))
})

test_that("a team that only faced weak defenses gets adjusted down", {
  # A padded its stats against WEAK; B faced tougher STRONG defenses.
  team_stats <- data.frame(
    team           = c("A", "A", "B", "B", "WEAK", "WEAK", "STRONG", "STRONG"),
    opponent_team  = c("WEAK", "WEAK", "STRONG", "STRONG", "A", "B", "A", "B"),
    season = 2024,
    passing_yards  = c(300, 300, 150, 150, 50, 50, 250, 250),
    rushing_yards = 0
  )
  ratings <- compute_opponent_adjusted_ratings(team_stats)

  raw_avg_A <- 300
  adj_A <- ratings$adj_offense[ratings$team == "A"]
  expect_true(adj_A < raw_avg_A)
})

test_that("a team that faced tough defenses gets adjusted up relative to its raw average", {
  team_stats <- data.frame(
    team           = c("A", "A", "B", "B", "WEAK", "WEAK", "STRONG", "STRONG"),
    opponent_team  = c("WEAK", "WEAK", "STRONG", "STRONG", "A", "B", "A", "B"),
    season = 2024,
    passing_yards  = c(300, 300, 150, 150, 50, 50, 250, 250),
    rushing_yards = 0
  )
  ratings <- compute_opponent_adjusted_ratings(team_stats)

  raw_avg_B <- 150
  adj_B <- ratings$adj_offense[ratings$team == "B"]
  expect_true(adj_B > raw_avg_B)
})

test_that("errors clearly when required columns are missing", {
  bad_stats <- data.frame(team = "A", opponent_team = "B")
  expect_error(compute_opponent_adjusted_ratings(bad_stats))
})

test_that("rows with NA offensive output are dropped, not propagated as NA ratings", {
  team_stats <- data.frame(
    team = c("A", "A", "B"),
    opponent_team = c("B", "B", "A"),
    season = 2024,
    passing_yards = c(200, NA, 200),
    rushing_yards = c(100, 100, 100)
  )
  ratings <- compute_opponent_adjusted_ratings(team_stats)
  expect_false(any(is.na(ratings$adj_offense)))
  expect_false(any(is.na(ratings$adj_defense)))
})
