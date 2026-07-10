test_that("prob_over_count decreases as the line rises", {
  p_low_line <- prob_over_count(5, line = 1, dispersion = 1)
  p_high_line <- prob_over_count(5, line = 9, dispersion = 1)
  expect_true(p_low_line > p_high_line)
  expect_true(p_low_line >= 0 && p_low_line <= 1)
})

test_that("prob_over_count at the mean is roughly a coin flip, not extreme", {
  p <- prob_over_count(5, line = 5, dispersion = 1)
  expect_true(p > 0.3 && p < 0.6)
})

test_that("prob_over_count handles zero and NA means safely", {
  expect_equal(prob_over_count(0, line = 1), 0)
  expect_equal(prob_over_count(NA, line = 1), 0)
})

test_that("prob_over_count with overdispersion is more spread out than plain Poisson", {
  # Same mean, but the negative-binomial (dispersion > 1) should assign more
  # probability mass to a line far above the mean than Poisson does.
  p_poisson <- prob_over_count(5, line = 10, dispersion = 1)
  p_negbinom <- prob_over_count(5, line = 10, dispersion = 2)
  expect_true(p_negbinom > p_poisson)
})

test_that("prob_over_yardage is monotonically decreasing in the line", {
  p1 <- prob_over_yardage(250, line = 200)
  p2 <- prob_over_yardage(250, line = 250)
  p3 <- prob_over_yardage(250, line = 300)
  expect_true(p1 > p2)
  expect_true(p2 > p3)
})

test_that("prob_over_yardage handles a non-positive line as a certain over", {
  expect_equal(prob_over_yardage(100, line = 0), 1)
})

test_that("prob_over_yardage handles a non-positive mean safely", {
  expect_equal(prob_over_yardage(0, line = 50), 0)
  expect_equal(prob_over_yardage(NA, line = 50), 0)
})

test_that("with_distribution bundles prediction/line/prob_over/prob_under summing to 1", {
  res <- with_distribution(250, line = 240, type = "yardage")
  expect_equal(res$prediction, 250)
  expect_equal(res$line, 240)
  expect_equal(res$prob_over + res$prob_under, 1)
})

test_that("with_distribution routes to the count distribution when asked", {
  res <- with_distribution(5, line = 4.5, type = "count", dispersion = 1)
  expect_equal(res$prob_over, prob_over_count(5, 4.5, dispersion = 1))
})
