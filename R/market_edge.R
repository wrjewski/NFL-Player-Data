# R/market_edge.R
#
# The old predict_game_edge() in app.R compared a model number to the
# spread/total with an arbitrary fixed threshold (">3 points", ">7 points")
# and never looked at the moneyline at all. The closing market line is
# usually the single best predictor of an NFL outcome, so "edge" should mean
# *disagreement with the market*, not just "my number differs from theirs by
# some arbitrary amount". This converts market prices to implied
# probabilities and compares them against the model's probability.

# American odds -> implied win probability (before removing the vig).
american_to_prob <- function(odds) {
  ifelse(odds > 0, 100 / (odds + 100), -odds / (-odds + 100))
}

# Removes the sportsbook's vig from a two-sided market so the two
# probabilities sum to exactly 1 ("de-vigging" / "no-vig" fair odds).
devig_two_way <- function(prob_a, prob_b) {
  total <- prob_a + prob_b
  list(prob_a = prob_a / total, prob_b = prob_b / total, overround = total - 1)
}

# model_prob:  the model's estimated probability of an outcome (e.g. home win,
#              or a prop clearing its line).
# market_prob: the de-vigged, market-implied probability of that same outcome.
# min_edge:    minimum absolute probability gap required to call it a real
#              edge instead of noise around the market's own estimate.
compute_edge <- function(model_prob, market_prob, min_edge = 0.03) {
  edge <- model_prob - market_prob
  side <- dplyr::case_when(
    is.na(edge) ~ NA_character_,
    edge >= min_edge ~ "Model favors this side vs. market",
    edge <= -min_edge ~ "Model favors the other side vs. market",
    TRUE ~ "No edge"
  )
  list(
    edge = edge,
    has_edge = !is.na(edge) && abs(edge) >= min_edge,
    side = side
  )
}

# Converts a predicted point-spread margin into a win probability using the
# normal approximation for NFL margin of victory (sd ~= 13.86 points, the
# commonly cited historical standard deviation of final NFL score margins).
# This is an approximation, not a fitted model -- good enough to compare a
# margin prediction against a moneyline, not precise enough to treat as
# calibrated on its own.
margin_to_win_prob <- function(pred_margin, margin_sd = 13.86) {
  stats::pnorm(pred_margin / margin_sd)
}
