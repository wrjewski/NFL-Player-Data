# 🏈 NFL Player Stats App (Shiny)

Welcome to the **NFL Player Stats App**, a web application built in **R using Shiny** that lets you explore NFL player statistics interactively.

This project is designed to be beginner-friendly — even if you've **never coded before**, this guide will help you understand what each part of the app does and how to get started.

---

## 📌 What This App Does

With this app, you can:

1. ✅ Select an NFL team.
2. ✅ Choose a position (like QB, RB, etc.).
3. ✅ Pick a player from the roster.
4. ✅ View detailed game-by-game stats for that player from 1999 to the current season.

All data comes from the [`nflreadr`](https://nflreadr.com/) package, which pulls real, up-to-date NFL data.

---

## 🧠 Tech Stack

| Tool        | Purpose                                      |
|-------------|----------------------------------------------|
| R           | The programming language used                |
| Shiny       | Used to build the interactive web app        |
| nflreadr    | Pulls NFL data from public APIs              |
| bslib       | Adds themes and styling to the app           |
| DT          | For displaying interactive data tables       |
| lubridate   | For handling dates and times in R            |

---

## 🗂️ Project Structure

NFL-Player-Data/
│
├── app.R # Main app file (UI + server)
├── modules/ # Modular code components
│ ├── ui_home.R # UI for team selection page
│ ├── ui_team.R # UI for offense/defense positions
│ ├── ui_position.R # UI for roster/player list
│ ├── ui_player.R # UI for player stats table
│ ├── server_player_stats.R # Code to render player stats
│
└── README.md # This file!


---

## 🚀 How To Run the App

### ✅ 1. Install Required R Packages

Open RStudio and run this once:

```r
install.packages(c("shiny", "nflreadr", "bslib", "DT", "lubridate"))

## Download the Project

1. Download the project folder (NFL-Player-Data) as a .zip file and unzip it
2. Or, clone it using Git if you are familiar with version control

## Run the App in RStudio

1. Open the NFL-Player-Data/app.R file in RStudio
2. Click Run App (top-right in the source editor), or run this command in your R console

```r
shiny::runApp("path/to/NFL-Player-Data")

replace path/to with the actual folder path on your machine

## App Overview for Beginners

| Component                | Purpose                                                                 |
|--------------------------|-------------------------------------------------------------------------|
| ui_home.R                | Displays NFL teams with logos you can click                             |
| ui_team.R                | Lets you pick an offense or defense position                            |
| ui_position.R            | Shows a list of players in the chosen position and team                 |
| ui_player.R              | Displays the detailed game-by-game stats for a player                   |
| server_player_stats.R    | Filters, formats, and renders player stats using the nflreadr package   |
| app.R                    | The main file that combines all parts and handles page navigation       |

## 🧠 How It All Works (In Simple Terms)

When you click a team, it sets your current page to "team" and updates the selected team.

When you click a position, it moves you to the "position" page and shows players at that position.

Clicking a player loads their stats using their unique ID.

The app uses reactive values to remember which team, position, or player you are looking at.

All of this is made possible by modular R files that each handle a small piece of the user interface or server logic.

## Common Issues

| Problem                               | Solution                                                                |
|---------------------------------------|-------------------------------------------------------------------------|
| App runs slow at first                | This is normal — it loads full player stats initially                   |
| no visible global function warning    | This is harmless. It is just a linting warning in VS Code or RStudio    |
| Player stats do not load              | The player may not have recorded any stats                              |
| Position shows “No players found”     | That position may not be assigned in current rosters                    |

## 💬 Frequently Asked Questions

💡 What if I don’t know any R?

No problem — just follow the steps to run the app. As you explore, reading the comments in each file will help you learn how things work.

💡 How can I add more stats or features?

You can open the server_player_stats.R file and update the switch() statement to include new stat columns based on position. It is all logic based on the players position string (e.g., "QB", "RB").

## 💻 Want to Learn More?

If you’re ready to explore further, here are some ideas:

📦 Convert the app into an R package using {golem}

🌍 Deploy the app to the internet with shinyapps.io

🧪 Add unit tests with {testthat}

🎨 Customize the theme more with {bslib}

## 🙌 Credits

Built using Shiny

Data provided by the excellent nflreadr project

## ✨ Final Thoughts

This app was built to help you learn. You don’t need to understand everything right away — just explore, break things, fix them, and you’ll learn faster than you think.

Happy coding! 🧠💻🏈