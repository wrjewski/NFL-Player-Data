test_that("american_to_prob converts favorites and underdogs correctly", {
  expect_equal(american_to_prob(-110), 110 / 210)
  expect_equal(american_to_prob(150), 100 / 250)
})

test_that("american_to_prob is vectorized over mixed favorite/underdog odds", {
  res <- american_to_prob(c(-120, 100, -300))
  expect_equal(length(res), 3)
  expect_equal(res[2], 0.5)
})

test_that("devig_two_way normalizes probabilities to sum to exactly 1", {
  p_home <- american_to_prob(-120)
  p_away <- american_to_prob(100)
  res <- devig_two_way(p_home, p_away)
  expect_equal(res$prob_a + res$prob_b, 1)
  expect_true(res$overround > 0) # a real two-sided book always has vig
})

test_that("compute_edge flags a real disagreement between model and market", {
  res <- compute_edge(model_prob = 0.60, market_prob = 0.50, min_edge = 0.03)
  expect_true(res$has_edge)
  expect_true(res$edge > 0)
  expect_equal(res$side, "Model favors this side vs. market")
})

test_that("compute_edge does not flag a small disagreement as an edge", {
  res <- compute_edge(model_prob = 0.51, market_prob = 0.50, min_edge = 0.03)
  expect_false(res$has_edge)
  expect_equal(res$side, "No edge")
})

test_that("compute_edge detects the other-side case symmetrically", {
  res <- compute_edge(model_prob = 0.40, market_prob = 0.50, min_edge = 0.03)
  expect_true(res$has_edge)
  expect_true(res$edge < 0)
  expect_equal(res$side, "Model favors the other side vs. market")
})

test_that("compute_edge handles NA probabilities without erroring", {
  res <- compute_edge(model_prob = NA, market_prob = 0.5)
  expect_false(res$has_edge)
  expect_true(is.na(res$side))
})

test_that("margin_to_win_prob returns 0.5 for a pick'em game", {
  expect_equal(margin_to_win_prob(0), 0.5)
})

test_that("margin_to_win_prob increases monotonically with predicted margin", {
  expect_true(margin_to_win_prob(7) > margin_to_win_prob(0))
  expect_true(margin_to_win_prob(-7) < margin_to_win_prob(0))
  expect_true(margin_to_win_prob(14) > margin_to_win_prob(7))
})

test_that("margin_to_win_prob stays within (0, 1)", {
  expect_true(margin_to_win_prob(100) < 1)
  expect_true(margin_to_win_prob(-100) > 0)
})
