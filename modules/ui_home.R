# ui_home.R

# This function is now used when the user clicks "Player Stats" from the main menu.
# It displays all NFL teams as clickable cards with their logos.

ui_home <- function(page, team) {
  teams <- nflreadr::load_teams()

  tagList(
    actionButton("back_home", "<- Back"),
    h2("Select a Team for Player Stats"),
    fluidRow(
      lapply(1:nrow(teams), function(i) {
        tr <- teams[i, ]
        logo_url <- tr$team_logo_wikipedia
        if (is.na(logo_url) || logo_url == "") logo_url <- NULL

        column(
          width = 2,
          actionButton(
            inputId = paste0("team_", tr$team_abbr),
            label = tags$div(
              if (!is.null(logo_url)) tags$img(src = logo_url, height = "60px"),
              tags$p(tr$team_name)
            ),
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