# R/distributions.R
#
# The old predict_* functions in app.R returned a single clamped number per
# stat. That's unusable for prop betting, where the question is "what's the
# probability this clears the sportsbook's line", not "what's the expected
# value". This turns a point estimate into a distribution and computes
# P(X > line) from it.

# P(X > line) for count stats (TDs, receptions), modeled as Negative
# Binomial by default since real per-game counts are overdispersed relative
# to Poisson; pass dispersion = 1 to fall back to plain Poisson.
prob_over_count <- function(mean_val, line, dispersion = 1.5) {
  if (is.na(mean_val) || mean_val <= 0) return(0)
  if (dispersion <= 1) {
    return(1 - stats::ppois(floor(line), lambda = mean_val))
  }
  size <- mean_val / (dispersion - 1)
  prob <- size / (size + mean_val)
  1 - stats::pnbinom(floor(line), size = size, prob = prob)
}

# P(X > line) for right-skewed continuous stats (yardage), modeled
# log-normal. cv (coefficient of variation = sd / mean) should be estimated
# from the player's recent-game sample; 0.45 is a reasonable default for
# skill-position yardage props absent a better estimate.
prob_over_yardage <- function(mean_val, line, cv = 0.45) {
  if (is.na(mean_val) || mean_val <= 0) return(0)
  if (line <= 0) return(1)
  sigma2 <- log(1 + cv^2)
  meanlog <- log(mean_val) - sigma2 / 2
  sdlog <- sqrt(sigma2)
  1 - stats::plnorm(line, meanlog = meanlog, sdlog = sdlog)
}

# Bundles a point prediction with its over/under probability for a given
# betting line, using whichever distribution family fits the stat type.
with_distribution <- function(mean_val, line, type = c("yardage", "count"), ...) {
  type <- match.arg(type)
  prob_over <- if (type == "count") {
    prob_over_count(mean_val, line, ...)
  } else {
    prob_over_yardage(mean_val, line, ...)
  }
  list(prediction = mean_val, line = line, prob_over = prob_over, prob_under = 1 - prob_over)
}
