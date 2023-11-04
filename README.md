
# chessclub

<!-- badges: start -->
<!-- badges: end -->

Calculate progressive Elo rankings for your team.

This shiny app takes a spreadsheet (CSV) of games played by players of a team (e.g. Chess), 
starts each player with an Elo score of 1000, then uses wins/losses/draws to calculate 
progressive Elo as more games are played.

Set the team name and k-factor (default 40) in `functions.R`, and add a
spreadsheet of played games as `games.csv` (example file included).

## Demo

[](images/01_allplayers.png)
[](images/02_individualplayers.png)
[](images/03_playmatrix.png)
[](images/04_winmatrix.png)
[](images/05_allgames.png)
[](images/06_upsets.png)

## Installation

This app currently requires the following packages:

```
  "dplyr",
  "DT",
  "elo",
  "ggplot2",
  "magrittr",
  "plotly",
  "rlang",
  "shiny",
  "shinydashboard",
  "tibble",
  "tidyr"
```


