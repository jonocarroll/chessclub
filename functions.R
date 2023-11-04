library(magrittr)

CLUB_NAME = "DEMO"
K_FACTOR = 40

reload <- function() {
  games <<- read.csv("games.csv")
  games$date <- as.Date(games$date)
  
  build_games <- function(d) {
    d <- dplyr::mutate(d, p1_elo = NA, .after = "p1_name")
    d <- dplyr::mutate(d, p2_elo = NA, .after = "p2_name")
    d <- dplyr::mutate(d, p1_prob_win = NA, .after = "p1_result")
    d <- dplyr::mutate(d, p1_new_elo = NA, .after = "p1_prob_win")
    d <- dplyr::mutate(d, p2_new_elo = NA, .after = "p1_new_elo")
    d
  }
  
  games <<- build_games(games)
  prior <- min(games$date) - 7
  
  build_players <- function(start_date = prior,
                            start_elo = 1000L) {
    unique_players <- sort(unique(c(games$p1_name, games$p2_name)))
    data.frame(date = start_date, name = unique_players, elo = start_elo)
  }
  
  players <<- build_players()
}

calc_elo <- function(rownum, k = K_FACTOR) {
  # find current elos
  gamedata <- games[rownum, ]
  
  # calculate new elos
  gamedata$p1_elo <- tail(players[players$name == gamedata$p1_name, "elo"], 1)
  gamedata$p2_elo <- tail(players[players$name == gamedata$p2_name, "elo"], 1)
  pred <- elo::elo.prob(gamedata$p1_elo, gamedata$p2_elo)
  gamedata$p1_prob_win <- as.integer(100*pred)
  result <- elo::elo.calc(gamedata$p1_result, gamedata$p1_elo, gamedata$p2_elo, k = k)
  gamedata$p1_new_elo <- as.integer(result$elo.A)
  gamedata$p2_new_elo <- as.integer(result$elo.B)
  
  # update player data
  players <<- rbind(players, data.frame(date = gamedata$date, 
                                        name = gamedata$p1_name, 
                                        elo = gamedata$p1_new_elo))
  players <<- rbind(players, data.frame(date = gamedata$date, 
                                        name = gamedata$p2_name, 
                                        elo = gamedata$p2_new_elo))
  
  # update game data
  games[rownum, ] <<- gamedata
}

table_elo <- function(player) {
  players[players$name == player, ]
}

chart_elo <- function(player) {
  ggplot2::ggplot(table_elo(player), ggplot2::aes(date, elo)) + 
    ggplot2::geom_line() +
    ggplot2::geom_point() + 
    ggplot2::theme_bw() + 
    ggplot2::labs(title = paste0("Player progress: ", player), 
                  x = "Date", y = "Elo")
}

all_charts <- function() {
  ggplot2::ggplot(players, ggplot2::aes(date, elo, col = name)) + 
    ggplot2::geom_line() +
    ggplot2::geom_point() + 
    ggplot2::theme_bw() + 
    ggplot2::labs(title = paste0("Player progress: ALL PLAYERS"), 
                  x = "Date", y = "Elo")
}

all_charts_plotly <- function() {
  gg <- players %>%
    plotly::highlight_key(~name) %>%
    ggplot2::ggplot(ggplot2::aes(date, elo, col = name)) + 
    ggplot2::geom_line() +
    ggplot2::geom_point() + 
    ggplot2::theme_bw() + 
    ggplot2::labs(title = paste0("Player progress: ALL PLAYERS"), 
                  x = "Date", y = "Elo")
  plotly::highlight(plotly::ggplotly(gg), on = "plotly_click")
}

current_elo <- function() {
  players %>%
    dplyr::group_by(name) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(elo)) %>%
    as.data.frame()
}

player_games <- function(player) {
  games[games$p1_name == player | games$p2_name == player, ]
}

find_pairs <- function(player = NULL) {
  p <- games[, c("p1_name", "p2_name")]
  p$a <- ifelse(p$p1_name < p$p2_name, p$p1_name, p$p2_name)
  p$b <- ifelse(p$p1_name < p$p2_name, p$p2_name, p$p1_name)
  res <- dplyr::count(p, a, b, sort = TRUE)
  if (!is.null(player)) {
    res[res$a == player | res$b == player, ]
  } else {
    res
  }
}

have_played <- function(p1, p2) {
  nrow(games[(games$p1_name == p1 & games$p2_name == p2) | 
               (games$p1_name == p2 & games$p2_name == p1), ]) > 0
}

largest_upsets <- function() {
  wins <- dplyr::slice_max(games[games$p1_result == 0 & games$p1_prob_win > 55, ], p1_prob_win, n = 5)
  losses <- dplyr::slice_min(games[games$p1_result == 1 & games$p1_prob_win < 45, ], p1_prob_win, n = 5)
  rbind(wins, losses)
}

prob_win <- function(p1, p2) {
  cur <- current_elo()
  as.integer(100*elo::elo.prob(cur[cur$name == p1, "elo"], cur[cur$name == p2, "elo"]))
}

win_matrix <- function() {
  pwv <- Vectorize(prob_win)
  cur <- current_elo()$name
  o <- outer(cur, cur, "pwv")
  rownames(o) <- cur
  colnames(o) <- cur
  o
}

plot_win_matrix <- function() {
wm <- win_matrix()
wm %>%
  as.data.frame() %>%
  tibble::rownames_to_column('player') %>%
  tidyr::pivot_longer(cols = -player, names_to = 'vs', values_to = 'prob') %>%
  ggplot2::ggplot() + ggplot2::aes(player, vs, fill = prob) + 
  ggplot2::geom_tile() + ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::scale_fill_viridis_c() +
  ggplot2::geom_hline(yintercept = seq(0.5, length(rownames(wm)), by = 1)) + 
  ggplot2::geom_vline(xintercept = seq(0.5, length(rownames(wm)), by = 1)) +
  ggplot2::theme(aspect.ratio = 1) +
  ggplot2::guides(x.sec = guide_axis_label_trans(~.x)) +
  ggplot2::guides(y.sec = guide_axis_label_trans(~.x)) +
  ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0)) 
}

play_matrix <- function() {
  pmv <- Vectorize(have_played)
  cur <- current_elo()$name
  o <- outer(cur, cur, "pmv")
  rownames(o) <- cur
  colnames(o) <- cur
  o
}

guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- ggplot2::guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}

plot_play_matrix <- function() {
  pm <- play_matrix()
  pm %>%
  as.data.frame() %>%
  tibble::rownames_to_column('player') %>%
  tidyr::pivot_longer(cols = -player, names_to = 'vs', values_to = 'played') %>%
  ggplot2::ggplot() + ggplot2::aes(player, vs, fill = played) + 
  ggplot2::geom_tile() + ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + 
  ggplot2::scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "steelblue")) + 
  ggplot2::geom_hline(yintercept = seq(0.5, length(rownames(pm)), by = 1)) + 
  ggplot2::geom_vline(xintercept = seq(0.5, length(rownames(pm)), by = 1)) +
  ggplot2::theme(aspect.ratio = 1) +
  ggplot2::guides(x.sec = guide_axis_label_trans(~.x)) +
  ggplot2::guides(y.sec = guide_axis_label_trans(~.x)) +
  ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0)) 
}
