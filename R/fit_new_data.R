#
# num_control <- 10
#
#
# library(tidyverse)
# library(emdbook)
# library(bezier)
# library(patchwork)
# library(tictoc)
# library(multidplyr)
# library(Matrix)
# library(here)
#
#
# games   <- read_csv("Data/games.csv",   col_types = cols())
# players <- read_csv("Data/players.csv", col_types = cols())
# plays   <- read_csv("Data/plays.csv",   col_types = cols())
#
#
# fit_new_data <- function(new_game_tracking_file){
#
#   type_position_cluster <- read_csv("deliverables/helper_functions/type_position_cluster.csv")
#
#
#
#   new_trajectories <-
#     type_position_cluster %>%
#     mutate(new_data_file = new_game_tracking_file,
#            position = str_to_upper(position),
#            situation = list(c("passing", "running"))) %>%
#     unnest() %>%
#     mutate(situ2 = if_else(situation == "passing", "Pass", "Run")) %>%
#     mutate(trajectories = pmap(list(new_data_file, type, situation, position, cluster_group, situ2),
#                                create_trajectories))
#
#   cluster_files <-
#     tibble(file_name_clust = list.files(paste0(here::here(), "/deliverables/cluster_centers"), full.names = T, recursive = T,)) %>%
#     mutate(size = str_extract(file_name_clust, "\\d{1,2}(?=\\.)"),
#            play_type = str_extract(file_name_clust, "(passing|running)"),
#            position = str_extract(file_name_clust, "(?<=(passing|running)\\/).+(?=\\/)"),
#            clust_data = map(file_name_clust, read_rds)) %>%
#     mutate_if(is.character, str_to_lower) %>%
#     select(-file_name_clust) %>%
#     mutate(clust_data_names = list(c("l_hood",
#                                      "Pik",
#                                      "Beta",
#                                      "Sigma",
#                                      "Alpha"))) %>%
#     unnest() %>%
#     spread(clust_data_names, clust_data)
#
#
#   new_piks <-
#     new_trajectories %>%
#     left_join(cluster_files, by = c("situation" = "play_type", "cluster_group" = "position")) %>%
#     drop_na(size) %>%
#     mutate(piks = pmap(list(trajectories, Alpha, Beta, Sigma),
#                        new_data_probs))
#
#   new_data_fitted <-
#     new_piks %>%
#     mutate(augmented_data = map2(piks, trajectories, possibly(augment_data, NA))) %>%
#     mutate(typ = map_chr(augmented_data, typeof)) %>%
#     select(size, play_type = situation, position  = cluster_group, augmented_data) %>%
#     mutate(t = map_chr(augmented_data, typeof)) %>%
#     filter(t != "logical") %>%
#     select(-t) %>%
#     unnest() %>%
#     spread(size, cluster)
#
#   return(new_data_fitted)
# }
#
#
#
# augment_data <- function(Pik, trajectories){
#
#   augmented_data <-
#     trajectories %>%
#     select(nflId, gameId, playId) %>%
#     bind_cols(as_tibble(Pik)) %>%
#     mutate(curve_i = row_number()) %>%
#     gather(cluster, prob, matches("\\d")) %>%
#     mutate(cluster = parse_number(cluster)) %>%
#     group_by(curve_i) %>%
#     filter(prob == max(prob)) %>%
#     filter(prob > 0.7) %>%
#     ungroup() %>%
#     select(-curve_i, -prob)
#
#   return(augmented_data)
# }
#
#
# new_data_probs <- function(routes_data, Alpha, Beta, Sigma){
#
#
#   P <- nrow(Beta[[1]]) - 1
#
#
#   # create data for em algorithm
#   routes_data <-
#     routes_data %>%
#     #dplyr::slice(1:100) %>%
#     mutate(curve_i = row_number()) %>%
#     mutate(n_i = map_dbl(data_same_sideline, nrow),
#            t_i = map(n_i, ~ tibble(t = (1:. - 1)/(.-1))),
#            T_i = map(t_i, ~ mutate(., p = list(0:P)) %>%
#                        unnest() %>%
#                        mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
#                        spread(p, D_p_of_t, sep = "_") %>%
#                        select(-t))) %>%
#     select(data_same_sideline, curve_i, n_i, t_i, T_i) %>%
#     arrange(curve_i) %>%
#     ungroup()
#
#
#   # create X matrix
#   X <-
#     routes_data %>%
#     select(T_i) %>%
#     unnest() %>%
#     Matrix::as.matrix()
#
#   # create Y matrix
#   Y <-
#     routes_data %>%
#     select(data_same_sideline) %>%
#     unnest() %>%
#     select(x, y) %>%
#     Matrix::as.matrix()
#
#   SEQ <-
#     routes_data %>%
#     select(n_i) %>%
#     mutate(n_i = cumsum(n_i)) %>%
#     Matrix::as.matrix()
#
#   INDEX <-
#     routes_data %>%
#     select(curve_i, t_i) %>%
#     unnest() %>%
#     select(curve_i)
#
#   n <- length(SEQ)
#   N <- SEQ[n]
#   curve_lengths <- routes_data %>% select(n_i)
#
#
#   calc_Piik <- function(data, Sigma){
#     data %>%
#       transmute(Piik = pmap_dbl(list(x, y, x1, y1), ~ dmvnorm(c(..1, ..2),
#                                                               c(..3, ..4),
#                                                               Sigma))) %>%
#       bind_cols(as_tibble(INDEX))
#   }
#
#
#   data_Piik <-
#     tibble(Beta, Sigma) %>%
#     mutate(k = row_number()) %>%
#     mutate(X_Beta = map(Beta,
#                         ~ Matrix::as.matrix(X) %*% .x %>%
#                           as_tibble() %>%
#                           bind_cols(as_tibble(Matrix::as.matrix(Y))))) %>%
#     mutate(Piik = map2(X_Beta, Sigma, calc_Piik)) %>%
#     select(k, Piik) %>%
#     unnest()
#
#   scale_m <-
#     data_Piik %>%
#     ungroup() %>%
#     summarise(mean = mean(Piik)) %>%
#     pull(mean)
#
#   Pik <-
#     data_Piik %>%
#     mutate(Piik = Piik/scale_m) %>%
#     group_by(curve_i, k) %>%
#     summarise(Pik = prod(Piik))  %>%
#     spread(k, Pik) %>%
#     ungroup() %>%
#     select(-curve_i) %>%
#     Matrix::as.matrix()
#
#   Pik <- Pik * Alpha
#
#   #############################################################################
#
#   # Calculate Log Likelihood
#
#   # Calculate Probability of data over all clusters
#   s <- rowSums(Pik)
#
#   # Since we're not on the log scale we might get 0
#   if(any(s == 0)){
#     # replace 0 with the smallest number possible
#     # then weight by the alphas
#     Pik[s == 0, ] <- .Machine$double.xmin * Alpha
#     # recalculate the probability of observing this data over all clusters
#     s <- rowSums(Pik)
#   }
#
#
#   # Calculate the Pi_ik
#   Pik <- Pik/s
#
#   return(Pik)
# }
#
#
#
# create_trajectories = function(new_data_file, type_player, situ, pos, lab, situ2){
#
#   pass_playIds <<-
#     plays %>%
#     filter(!isSTPlay) %>%
#     drop_na(PassResult) %>%
#     dplyr::select(gameId, playId, quarter)
#
#   # Extract which plays were rushesn
#   rush_playIds <<-
#     plays %>%
#     filter(!isSTPlay) %>%
#     filter(is.na(PassResult)) %>%
#     filter(!str_detect(playDescription, "TWO-POINT") & !str_detect(playDescription, "pass")) %>%
#     dplyr::select(gameId, playId, quarter)
#
#   play_ids <-
#     if(situ == "passing"){
#       pass_playIds
#     } else{
#       rush_playIds
#     }
#
#   # make a key to find player position
#   player_pos_id_key <-
#     players %>% dplyr::select(nflId, PositionAbbr)
#
#   # subset that group to only grab the ones who "run routes often?
#   route_runners_pos_id_key <<-
#     player_pos_id_key %>%
#     filter(PositionAbbr %in% c(pos))
#
#   route_over_event <<-
#     c(# "pass_arrived",
#       "pass_outcome_caught",
#       "pass_outcome_incomplete",
#       "qb_sack",
#       "run",
#       # "pass_tipped",
#       "touchdown",
#       "pass_outcome_interception",
#       "pass_outcome_touchdown",
#       "fumble",
#       "qb_strip_sack",
#       # "fumble_defense_recovered",
#       "pass_shovel",
#       "handoff",
#       # "fumble_offense_recovered",
#       "qb spike")
#
#   # tp get the events
#   # routes_data %>% slice(1:500) %>% unnest(data) %>% count(event) %>% pull(event) %>% dput()
#
#   # where to end routes
#   run_over_event <<-
#     c(#"ball_snap",
#       "first_contact",
#       "fumble",
#       #"handoff",
#       #"lateral",
#       #"line_set",
#       #"man_in_motion",
#       "out_of_bounds",
#       "qb_kneel",
#       "run",
#       #"shift",
#       #"snap_direct",
#       "tackle",
#       "touchdown")
#
#   route_over_event <<-
#     if(situ == "passing"){
#       route_over_event
#     } else{
#       run_over_event
#     }
#
#   route_begin_event <<- "ball_snap"
#
#
#   # list all the files
#   tracking_files <- list.files(path = "Data/", pattern = "tracking_.*\\.csv")
#
#   # Load all the data
#   routes_data <-
#     # create the tibble with the file names
#     tibble(file_name = new_data_file) %>%
#     # actually just the first file for now
#     # dplyr::slice(1) %>%
#     # read the data in nested
#     mutate(data = map(file_name, ~ read_routes_from_csv_test(., play_type = situ2, player_type = type_player))) %>%
#     dplyr::select(-file_name) %>%
#     unnest()
#
#
#   #################################################################################33
#   # where to end routes
#
#
#   ## How many control points is 10
#   # 10 - 1 is degree 9
#   P <<- num_control
#
#
#
#
#
#   # transform our curves
#   routes_data <-
#     routes_data %>%
#     mutate(row = row_number()) %>%
#     mutate(data = pmap(list(data, team, direction_left, line_of_scrimmage),
#                        ~ cut_plays(..1) %>%
#                          flip_field(., ..2, ..3, ..4)),
#            n = map_dbl(data, nrow)) %>%
#     filter(n >= num_control * 2) %>%
#     # left side of field is TRUE
#     mutate(data_same_sideline = purrr::map(data,
#                                            ~ mutate(.,
#                                                     sof = 160/6 > first(y),
#                                                     y = if_else(sof, 160/3 - y, y),
#                                                     y = y - first(y)
#                                            ) %>%
#                                              dplyr::select(-sof)))  %>%
#     arrange(row)
#
#
#   routes_data <-
#     routes_data %>%
#     ungroup() %>%
#     select(-row)
#
#   # save(routes_data, file = "TEtraj.Rdata")
#
#   return(routes_data)
# }
#
#
#
# # create a new reading function
# read_routes_from_csv_test <- function(file_name, play_type="Pass", player_type = "Offense"){
#   # Function could be made more secure by throwing error if not Pass or Rush
#   # For now, either specify Pass for Pass or literally anything else for rush
#   if(play_type %in% "Pass"){
#     data <-
#       # read all of the data in
#       read_csv(file_name, col_types = cols()) %>%
#       # drop unnescceary columns
#       dplyr::select(., nflId, gameId, playId, x, y, frame.id, team, event) %>%
#       # keep only the passing plays
#       inner_join(., pass_playIds, by = c("gameId", "playId"))
#
#   } else{
#     data <-
#       # read all of the data in
#       read_csv(file_name, col_types = cols()) %>%
#       # drop unnescceary columns
#       dplyr::select(., nflId, gameId, playId, x, y, frame.id, team, event) %>%
#       # keep only the passing plays
#       inner_join(., rush_playIds, by = c("gameId", "playId"))
#
#   }
#
#   # USe this one for people on the line of scrimmage at ball snap
#
#   line_of_scrimmage <-
#     data %>%
#     filter(event == route_begin_event) %>%
#     group_by(gameId, playId, team) %>%
#     summarise(right_scrim = max(x), left_scrim = min(x))
#
#   play_direction <-
#     data %>%
#     filter(event == route_begin_event) %>%
#     group_by(gameId, playId, team) %>%
#     summarise(mean_team = mean(x)) %>%
#     filter(team != "ball") %>%
#     filter(mean_team == max(mean_team)) %>%
#     dplyr::select(gameId, playId, direction_left = team, -mean_team)
#
#   possesion <-
#     plays %>%
#     dplyr::select(gameId, playId, possessionTeam) %>%
#     left_join(games, by = "gameId") %>%
#     mutate(possesion = if_else(possessionTeam == homeTeamAbbr, "home", "away")) %>%
#     dplyr::select(gameId, playId, possesion)
#
#
#   if(player_type %in% "Offense"){
#     data <-
#       data %>%
#       # keep only the tracking data for route runners
#       inner_join(., route_runners_pos_id_key, by = c("nflId")) %>%
#       # nest the x, y data for more consciness
#       nest(-nflId, -gameId, -playId, -PositionAbbr, -team, -quarter) %>%
#       left_join(play_direction, by = c("gameId", "playId")) %>%
#       left_join(line_of_scrimmage, by = c("gameId", "playId", "team")) %>%
#       left_join(possesion, by = c("gameId", "playId")) %>%
#       filter(team == possesion) %>%
#       mutate(line_of_scrimmage = if_else(team == direction_left, left_scrim, right_scrim)) %>%
#       dplyr::select(-right_scrim, -left_scrim, -possesion)
#   } else{
#     data <-
#       data %>%
#       # keep only the tracking data for route runners
#       inner_join(., route_runners_pos_id_key, by = c("nflId")) %>%
#       # nest the x, y data for more consciness
#       nest(-nflId, -gameId, -playId, -PositionAbbr, -team, -quarter) %>%
#       left_join(play_direction, by = c("gameId", "playId")) %>%
#       left_join(line_of_scrimmage, by = c("gameId", "playId", "team")) %>%
#       left_join(possesion, by = c("gameId", "playId")) %>%
#       filter(team != possesion) %>%
#       mutate(line_of_scrimmage = if_else(team == direction_left, left_scrim, right_scrim)) %>%
#       dplyr::select(-right_scrim, -left_scrim, -possesion)
#   }
#
#
#
#
#   return(data)
# }
#
#
# cut_plays <- function(data){
#   data <-
#     mutate(data,
#            # Give a second ahead of the play start so we filter extraneous movement
#            # but capture pre-snap movement
#            play_group = case_when(dplyr::lead(event, 10) %in% route_begin_event ~ 1,
#                                   #event == "ball_snap"             ~ 1,
#                                   # use lag so we can see which event
#                                   # cut off the route
#                                   dplyr::lag(event) %in% route_over_event   ~ 1,
#                                   TRUE                               ~ 0),
#            # pre snap should be 0
#            # after snap, during route should be 1,
#            # anything greater than 1 is after the route is over
#            play_group = cumsum(play_group)) %>%
#     filter(play_group == 1) %>%
#     dplyr::select(-play_group)
# }
#
#
# flip_field <- function(data, team, direction_left, line_of_scrimmage){
#   mutate(data,
#          # flip the field to try and get all routes in the same direction
#          dir = team == direction_left,
#          x   = if_else(dir, 120 - x, x),
#          y   = if_else(dir, 160/3 - y, y),
#          # make the line of scrimmage start at the same place for every route.
#          x   = if_else(dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) %>%
#     drop_na(x)
# }
#
#
