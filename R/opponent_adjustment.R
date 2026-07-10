# R/opponent_adjustment.R
#
# The old team_summary in app.R compared teams using raw season averages,
# which conflates a team's real strength with the strength of the schedule
# it happened to face. This computes a lightweight opponent-adjusted rating
# instead: each team's offensive/defensive output is iteratively rescaled by
# the strength of the opponents it actually played (a simplified
# SRS/Massey-style power rating), so a big game against a bad defense counts
# for less than the same output against a good one.

compute_opponent_adjusted_ratings <- function(team_stats, iterations = 4) {
  required_cols <- c("team", "opponent_team", "season", "passing_yards", "rushing_yards")
  stopifnot(all(required_cols %in% names(team_stats)))

  per_game <- team_stats %>%
    dplyr::mutate(off_total = passing_yards + rushing_yards) %>%
    dplyr::select(team, opponent_team, season, off_total) %>%
    dplyr::filter(!is.na(off_total))

  teams <- sort(unique(c(per_game$team, per_game$opponent_team)))
  league_avg <- mean(per_game$off_total, na.rm = TRUE)

  off_rating <- stats::setNames(rep(league_avg, length(teams)), teams)
  def_rating <- stats::setNames(rep(league_avg, length(teams)), teams)

  for (i in seq_len(iterations)) {
    new_off <- vapply(teams, function(tm) {
      games <- per_game[per_game$team == tm, ]
      if (nrow(games) == 0) return(league_avg)
      mean(games$off_total * (league_avg / def_rating[games$opponent_team]), na.rm = TRUE)
    }, numeric(1))

    new_def <- vapply(teams, function(tm) {
      games <- per_game[per_game$opponent_team == tm, ]
      if (nrow(games) == 0) return(league_avg)
      mean(games$off_total * (league_avg / off_rating[games$team]), na.rm = TRUE)
    }, numeric(1))

    off_rating <- new_off
    def_rating <- new_def
  }

  tibble::tibble(
    team = teams,
    adj_offense = as.numeric(off_rating),
    adj_defense = as.numeric(def_rating),
    league_avg_offense = league_avg
  )
}
