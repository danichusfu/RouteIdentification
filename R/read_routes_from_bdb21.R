

#' read in route data from tracking data csv
#'
#' @param file_name name of the tracking data file
#' @return The routes from the route runner from the tracking file
#' @importFrom magrittr %>%
#' @export

# create a new reading function
read_routes_from_bdb21 <- function(file_name){
  
  # Load Basic Data
  games   <- readr::read_csv("C:/Users/Dani Chu/Documents/big_data_bowl_2021/data/games.csv",   col_types = cols())
  players <- readr::read_csv("C:/Users/Dani Chu/Documents/big_data_bowl_2021/data/players.csv", col_types = cols())
  plays   <- readr::read_csv("C:/Users/Dani Chu/Documents/big_data_bowl_2021/data/plays.csv",   col_types = cols())
  

  # make a key to find player position
  player_pos_id_key <-
    players %>% dplyr::select(nflId, position)
  
  # subset that group to only grab the ones who "run routes often?
  route_runners_pos_id_key <-
    player_pos_id_key %>%
    dplyr::filter(position %in%c("CB", "DB", "DE", "DL", "FS", "ILB", "LB", "MLB", 
                                 "NT", "OLB", "S", "SS"))
  
  data <-
    # read all of the data in
    readr::read_csv(file_name, col_types = cols()) %>%
    # drop unnescceary columns
    dplyr::select(., nflId, gameId, playId, x, y, frameId, team, event, jerseyNumber, displayName)
  
  play_direction <-
    data %>%
    dplyr::filter(event == "ball_snap") %>%
    dplyr::group_by(gameId, playId, team) %>%
    dplyr::summarise(mean_team = mean(x)) %>%
    dplyr::filter(team != "football") %>%
    dplyr::filter(mean_team == max(mean_team)) %>%
    dplyr::select(gameId, playId, direction_left = team, -mean_team)
  
  possesion <-
    plays %>%
    dplyr::select(gameId, playId, possessionTeam) %>%
    dplyr::left_join(games, by = "gameId") %>%
    dplyr::mutate(possesion = if_else(possessionTeam == homeTeamAbbr, "home", "away")) %>%
    dplyr::select(gameId, playId, possesion)
  
  line_of_scrimmage <-
    data %>%
    dplyr::filter(event == "ball_snap") %>%
    dplyr::group_by(gameId, playId, team) %>%
    dplyr::summarise(right_scrim = max(x), left_scrim = min(x))
  
  data <-
    data %>%
    # keep only the tracking data for route runners
    dplyr::inner_join(., route_runners_pos_id_key, by = c("nflId")) %>%
    # nest the x, y data for more consciness
    tidyr::nest(data = c(x, y, frameId, event, jerseyNumber)) %>%
    dplyr::left_join(play_direction, by = c("gameId", "playId")) %>%
    dplyr::left_join(line_of_scrimmage, by = c("gameId", "playId", "team")) %>%
    dplyr::left_join(possesion, by = c("gameId", "playId")) %>%
    dplyr::filter(team != possesion) %>%
    dplyr::mutate(line_of_scrimmage = dplyr::if_else(team == direction_left, left_scrim, right_scrim)) %>%
    dplyr::select(-right_scrim, -left_scrim, -possesion)
  
  
  return(data)
}
