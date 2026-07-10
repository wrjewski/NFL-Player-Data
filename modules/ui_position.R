# This function builds the UI for showing
# all players in a selected position on a selected team.
# Users can click a player to view their game log and stats.

ui_position <- function(page, team, pos, player) {
  # Ensure a team and position are selected before displaying this UI
  req(team(), pos()) # nolint

  # Load the full NFL rosters dataset
  rosters <- nflreadr::load_rosters()

  # Filter players who are on the selected team and play the selected position
  players <- rosters[rosters$team == team() & rosters$position == pos(), ]

  # Start building the UI for this screen
  tagList( # nolint
    # Button to go back to the team screen
    actionButton("back_team", "<- Back"), # nolint

    # Heading showing what position and team the user is viewing
    h3(paste("Position:", pos(), "on", team())), # nolint

    # If there are no players for that position on this team, show a message
    if (nrow(players) == 0) {
      p("No players found.")  # Displayed when no players match # nolint
    } else {
      # Otherwise, show a list of buttons—one for each player
      tagList( # nolint
        lapply(1:nrow(players), function(i) { # nolint
          # Unique ID for each player button
          p_btn <- paste0("player_", players$gsis_id[i])

          # Create the button with the player’s jersey number and name
          actionButton( # nolint
            inputId = p_btn,
            label = paste(players$jersey_number[i], "-", players$full_name[i]),
            style = "
              width: 100%;                 # Button takes full width
              margin-bottom: 6px;          # Space between buttons
              background-color: white;     # White background
              border: 1px solid #000;      # Black border
              text-align: left;            # Align text to the left
              color: black;"               # Black text color
          )
        })
      )
    }
  )
}
