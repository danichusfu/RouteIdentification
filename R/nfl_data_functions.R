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

#' read in route data from tracking data csv
#'
#' @param file_name name of the tracking data file
#' @return The routes from the route runner from the tracking file

# create a new reading function
read_routes_from_903124 <- function(file_name){

  # games   <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv",   col_types = cols())
  # players <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv", col_types = cols())
  # plays   <- read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv",   col_types = cols())
  # 
  data <-
    # read all of the data in
    read_csv(file_name, col_types = cols()) 
  
  data <-
    data %>%
    # Improved so their are no longer duplicates now
    distinct() %>%
    # drop unnescceary columns
    #dplyr::select(., nflId, gameId = game_id, playId = play_id, x, y, event = event_name, position) %>%
    dplyr::rename(gameId = game_id, playId = play_id, event = event_name) %>%
    # keep only the passing plays
    group_by(gameId, playId) %>%
    mutate(pass_play = sum(event %in% "pass_forward") >= 1,
           offense   = position %in% c("C", "FB","G", 
                                       "NT", "OG", "OT", "QB", "RB", 
                                       "T", "TE", "WR")) %>%
    filter(pass_play, offense)


  play_direction <-
    data %>%
    group_by(gameId, playId) %>%
    filter(event %in% c("ball_snap", "snap_direct") | time == max(time)) %>%
    group_by(gameId, playId, time) %>%
    summarise(mean_team = mean(x)) %>%
    mutate(time = c("ball_snap", "end_play")) %>%
    pivot_wider(names_from = time, values_from = mean_team) %>%
    mutate(left = end_play < ball_snap) %>%
    select(gameId, playId, left)

  line_of_scrimmage <-
    data %>%
    filter(event == "ball_snap") %>%
    group_by(gameId, playId) %>%
    summarise(right_scrim = max(x), left_scrim = min(x))

  data <-
    data %>%
    # keep only the tracking data for route runners
    filter(position %in% c("RB", "WR", "TE")) %>%
    select(-X1, -dir, -o, - s, -pass_play, -offense) %>%
    # nest the x, y data for more consciness
    nest(data = c(x, y, time, event)) %>%
    left_join(play_direction, by = c("gameId", "playId")) %>%
    left_join(line_of_scrimmage, by = c("gameId", "playId")) %>%
    mutate(line_of_scrimmage = if_else(left, left_scrim, right_scrim)) %>%
    dplyr::select(-right_scrim, -left_scrim)


  return(data)
}


#' cut plays to only get the route information we care about
#'
#' @param data routes from the tracking data
#' @return the routes cut up at the appropriate places

cut_plays <- function(data){


  #################################################################################33
  # where to end routes
  route_over_event <-
    c(# "pass_arrived",
      "pass_outcome_caught",
      "pass_outcome_incomplete",
      "qb_sack",
      "run",
      # "pass_tipped",
      "touchdown",
      "pass_outcome_interception",
      "pass_outcome_touchdown",
      "fumble",
      "qb_strip_sack",
      # "fumble_defense_recovered",
      "pass_shovel",
      "handoff",
      # "fumble_offense_recovered",
      "qb spike")


  data <-
    mutate(data,
           play_group = case_when(event == "ball_snap"             ~ 1,
                                  # use lag so we can see which event
                                  # cut off the route
                                  dplyr::lag(event) %in% route_over_event   ~ 1,
                                  TRUE                               ~ 0),
           # pre snap should be 0
           # after snap, during route should be 1,
           # anything greater than 1 is after the route is over
           play_group = cumsum(play_group)) %>%
    filter(play_group == 1) %>%
    dplyr::select(-play_group)

}

#' flip field about the split line
#'
#' @param data routes from the tracking data
#' @param team the team the player plays on
#' @param direction_left the team that is going in the left direction
#' @param line_of_scrimmage what the line of scrimmage is for tat play
#' @return return te data that has been flipped about the split line
flip_field <- function(data, team, direction_left, line_of_scrimmage){
  mutate(data,
         # flip the field to try and get all routes in the same direction
         dir = team == direction_left,
         x   = if_else(dir, 120 - x, x),
         y   = if_else(dir, 160/3 - y, y),
         # make the line of scrimmage start at the same place for every route.
         x   = if_else(dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) %>%
    drop_na(x)
}

#' flip field about the split line
#'
#' @param data routes from the tracking data
#' @param left the team the player plays on
#' @param line_of_scrimmage what the line of scrimmage is for tat play
#' @return return te data that has been flipped about the split line
flip_field_903124 <- function(data, left, line_of_scrimmage){
  mutate(data,
         # flip the field to try and get all routes in the same direction
         dir = left,
         x   = if_else(dir, 120 - x, x),
         y   = if_else(dir, 160/3 - y, y),
         # make the line of scrimmage start at the same place for every route.
         x   = if_else(dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) %>%
    drop_na(x)
}
