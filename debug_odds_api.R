# Debug the odds API
library(httr)
library(jsonlite)

# Key is read from the ODDS_API_KEY environment variable (see .Renviron.example).
if (file.exists(".Renviron")) readRenviron(".Renviron")
odds_api_key <- Sys.getenv("ODDS_API_KEY")
if (identical(odds_api_key, "")) {
  stop("ODDS_API_KEY is not set. Copy .Renviron.example to .Renviron and add your key.")
}
odds_base <- "https://api.the-odds-api.com/v4"

# Test the main odds endpoint
cat("Testing main odds endpoint...\n")
resp <- GET(
  url = paste0(odds_base, "/sports/americanfootball_nfl/odds"),
  query = list(
    apiKey = odds_api_key,
    regions = "us",
    markets = "h2h,spreads,totals",
    oddsFormat = "american"
  )
)

cat("Status code:", status_code(resp), "\n")
if (status_code(resp) == 200) {
  cat("Success! Got odds data\n")
  data <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data, flatten = TRUE)
  cat("Number of games:", length(parsed), "\n")
  if (length(parsed) > 0) {
    cat("First game ID:", parsed[[1]]$id, "\n")
  }
} else {
  cat("Error:", content(resp, as = "text"), "\n")
}

# Test player props endpoint with a sample game ID
cat("\nTesting player props endpoint...\n")
# Use a sample game ID from the schedule
sample_game_id <- "2025_01_DAL_PHI"  # This should exist in our schedule
resp2 <- GET(
  url = paste0(odds_base, "/events/", sample_game_id, "/odds"),
  query = list(
    apiKey = odds_api_key,
    regions = "us",
    markets = "player_pass_yds,player_rush_yds,player_receptions",
    oddsFormat = "american"
  )
)

cat("Player props status code:", status_code(resp2), "\n")
if (status_code(resp2) == 200) {
  cat("Success! Got player props data\n")
  data2 <- content(resp2, as = "text", encoding = "UTF-8")
  parsed2 <- fromJSON(data2, flatten = TRUE)
  cat("Number of events:", length(parsed2), "\n")
} else {
  cat("Player props error:", content(resp2, as = "text"), "\n")
}
