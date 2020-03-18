

#' read in route data from tracking data csv
#'
#' @param file_name name of the tracking data file
#' @return The routes from the route runner from the tracking file
#' @export

# create a new reading function
read_routes_from_903124 <- function(file_name){
  
  # games   <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv",   col_types = cols())
  # players <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv", col_types = cols())
  # plays   <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv",   col_types = cols())
  # 
  data <-
    # read all of the data in
    readr::read_csv(file_name, col_types = cols()) 
  
  data <-
    data %>%
    # Improved so there are no longer duplicates now
    dplyr::distinct() %>%
    # drop unnescceary columns
    #dplyr::select(., nflId, gameId = game_id, playId = play_id, x, y, event = event_name, position) %>%
    dplyr::rename(gameId = game_id, playId = play_id, event = event_name, displayName = displayName) %>%
    dplyr::mutate(team = NA) %>% # temp fix
    # keep only the passing plays
    dplyr::group_by(gameId, playId) %>%
    dplyr::mutate(pass_play = sum(event %in% "pass_forward") >= 1,
           offense   = position %in% c("C", "FB","G", 
                                       "NT", "OG", "OT", "QB", "RB", 
                                       "T", "TE", "WR")) %>%
    dplyr::filter(pass_play, offense)
  
  
  play_direction <-
    data %>%
    dplyr::group_by(gameId, playId) %>%
    dplyr::filter(event %in% c("ball_snap", "snap_direct") | time == max(time)) %>%
    dplyr::group_by(gameId, playId, time) %>%
    dplyr::summarise(mean_team = mean(x)) %>%
    dplyr::mutate(time = c("ball_snap", "end_play")) %>%
    tidyr::pivot_wider(names_from = time, values_from = mean_team) %>%
    dplyr::mutate(left = end_play < ball_snap) %>%
    dplyr::select(gameId, playId, direction_left = left)
  
  line_of_scrimmage <-
    data %>%
    dplyr::filter(event == "ball_snap") %>%
    dplyr::group_by(gameId, playId) %>%
    dplyr::summarise(right_scrim = max(x), left_scrim = min(x))
  
  data <-
    data %>%
    # keep only the tracking data for route runners
    dplyr::filter(position %in% c("RB", "WR", "TE")) %>%
    dplyr::select(-X1, -dir, -o, - s, -pass_play, -offense) %>%
    # nest the x, y data for more consciness
    tidyr::nest(data = c(x, y, time, event)) %>%
    dplyr::left_join(play_direction, by = c("gameId", "playId")) %>%
    dplyr::left_join(line_of_scrimmage, by = c("gameId", "playId")) %>%
    dplyr::mutate(line_of_scrimmage = dplyr::if_else(direction_left, left_scrim, right_scrim)) %>%
    dplyr::select(-right_scrim, -left_scrim)
  
  return(data)
}
