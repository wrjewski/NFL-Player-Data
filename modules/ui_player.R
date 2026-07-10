# This function builds the UI for the player page.
# It shows the game log (table of stats) for a selected player.

ui_player <- function(page, player_id) {
  # Ensure a player has been selected before rendering the UI
  req(player_id()) # nolint

  # Return the user interface components for the player page
  tagList( # nolint
    # Button to go back to the position selection screen
    actionButton("back_position", "<- Back"), # nolint

    # Header title for the page
    h3("Player Game Log"), # nolint

    # Output area where the player’s stats table will appear
    # This connects to the server-side output called 'player_stats_table'
    DT::dataTableOutput("player_stats_table")
  )
}
