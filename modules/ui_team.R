# This function defines the UI for the team-specific page.
# It displays all the offensive and defensive positions as buttons.
# When a user clicks a position button
# they’ll move to the player selection screen.

ui_team <- function(page, team, pos, offense_positions, defense_positions) {
  tagList(  # Groups multiple UI elements together # nolint
    actionButton("back_home", "<- Back"),  # Button to return to the home screen # nolint
    h3(paste("Team:", team())),  # Display the selected team name as a heading # nolint

    # --- Offense Section ---
    h4("Offense"),  # Heading for offensive positions # nolint
    fluidRow(  # Create a row layout for buttons # nolint
      # Loop through each offensive position
      lapply(offense_positions, function(pos_name) {
        column(  # Each button goes in its own column # nolint
          width = 2,  # Control the width of each button column
          actionButton( # nolint
            inputId = paste0("pos_", pos_name),  # Unique ID like "pos_QB"
            label = pos_name,  # Button label (e.g., "QB", "WR")
            style = "  # Custom styles for the button
              width: 100%;              # Button takes full column width
              margin-bottom: 10px;      # Space between buttons
              background-color: #f8f9fa;  # Light gray background
              border: 1px solid black;  # Black border
              color: black;"            # Black text
          )
        )
      })
    ),

    # --- Defense Section ---
    h4("Defense"),  # Heading for defensive positions
    fluidRow(  # Another row for defense
      # Loop through each defensive position
      lapply(defense_positions, function(pos_name) {
        column( # nolint
          width = 2,
          actionButton( # nolint
            inputId = paste0("pos_", pos_name),  # Unique ID like "pos_LB"
            label = pos_name,
            style = "
              width: 100%;
              margin-bottom: 10px;
              background-color: white;
              border: 1px solid black;
              color: black;"
          )
        )
      })
    )
  )
}
