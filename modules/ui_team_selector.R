# This function builds the UI for the home page.
# It shows all NFL teams as clickable buttons with their logos and names. 

ui_home <- function(page, team) { 
    
    # Load all NFL teams using nflreadr 
    teams <- nflreadr::load_teams() 
    # Build and return the UI components 
    tagList( # nolint 
        # Title at the top of the home page
        h2("Select a team"), # nolint
        # Arrange the team buttons in a responsive row layout
        fluidRow( # nolint
        # Loop through each team (one row per team)
        lapply(1:nrow(teams), function(i) { # nolint
            tr <- teams[i, ] # Extract the current team's data
            logo_url <- tr$team_logo_wikipedia # Get logo URL
            
            # If logo is missing or blank, set to NULL to avoid rendering an image
            if (is.na(logo_url) || logo_url == "") logo_url <- NULL
            
            # Create a column for each team (width 2 out of 12 in Bootstrap grid) 
            column( # nolint
            width = 2,
            # Create an action button for each team
            actionButton( # nolint
                # Unique button ID per team
                inputId = paste0("team_", tr$team_abbr),
                label = tags$div( # nolint
                # Show logo if available
                if (!is.null(logo_url)) tags$img(src = logo_url, height = "60px"),
                # Show team name
                tags$p(tr$team_name) 
            ),
            # Custom button styling: white background, black border, full width 
            style = "
                width: 100%;
                height: 120px;
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