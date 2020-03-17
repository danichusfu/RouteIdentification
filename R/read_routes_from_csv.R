

#' read in route data from tracking data csv
#'
#' @param file_name name of the tracking data file
#' @return The routes from the route runner from the tracking file

# create a new reading function
read_routes_from_csv <- function(file_name){
  
  # Load Basic Data
  games   <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv",   col_types = cols())
  players <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv", col_types = cols())
  plays   <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv",   col_types = cols())
  
  
  # Extract which plays were "passes", had dropbacks I think is a better definition
  pass_playIds <-
    plays %>%
    tidyr::drop_na(PassResult) %>%
    dplyr::select(gameId, playId, quarter)
  
  # make a key to find player position
  player_pos_id_key <-
    players %>% dplyr::select(nflId, PositionAbbr)
  
  # subset that group to only grab the ones who "run routes often?
  route_runners_pos_id_key <-
    player_pos_id_key %>%
    dplyr::filter(PositionAbbr %in% c("WR", "TE", "RB", "FB"))
  
  data <-
    # read all of the data in
    readr::read_csv(file_name, col_types = cols()) %>%
    # drop unnescceary columns
    dplyr::select(., nflId, gameId, playId, x, y, frame.id, team, event, jerseyNumber) %>%
    # keep only the passing plays
    dplyr::inner_join(., pass_playIds, by = c("gameId", "playId"))
  
  play_direction <-
    data %>%
    filter(event == "ball_snap") %>%
    group_by(gameId, playId, team) %>%
    summarise(mean_team = mean(x)) %>%
    filter(team != "ball") %>%
    filter(mean_team == max(mean_team)) %>%
    dplyr::select(gameId, playId, direction_left = team, -mean_team)
  
  possesion <-
    plays %>%
    dplyr::select(gameId, playId, possessionTeam) %>%
    left_join(games, by = "gameId") %>%
    mutate(possesion = if_else(possessionTeam == homeTeamAbbr, "home", "away")) %>%
    dplyr::select(gameId, playId, possesion)
  
  line_of_scrimmage <-
    data %>%
    filter(event == "ball_snap") %>%
    group_by(gameId, playId, team) %>%
    summarise(right_scrim = max(x), left_scrim = min(x))
  
  data <-
    data %>%
    # keep only the tracking data for route runners
    inner_join(., route_runners_pos_id_key, by = c("nflId")) %>%
    # nest the x, y data for more consciness
    nest(data = c(x, y, frame.id, event, jerseyNumber)) %>%
    left_join(play_direction, by = c("gameId", "playId")) %>%
    left_join(line_of_scrimmage, by = c("gameId", "playId", "team")) %>%
    left_join(possesion, by = c("gameId", "playId")) %>%
    filter(team == possesion) %>%
    mutate(line_of_scrimmage = if_else(team == direction_left, left_scrim, right_scrim)) %>%
    dplyr::select(-right_scrim, -left_scrim, -possesion)
  
  
  return(data)
}
