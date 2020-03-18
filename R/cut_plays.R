

#' cut plays to only get the route information we care about
#'
#' @param data routes from the tracking data
#' @importFrom magrittr %>% 
#' @return the routes cut up at the appropriate places
#' @export

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
    dplyr::mutate(data,
           play_group = case_when(event == "ball_snap"             ~ 1,
                                  # use lag so we can see which event
                                  # cut off the route
                                  dplyr::lag(event) %in% route_over_event   ~ 1,
                                  TRUE                               ~ 0),
           # pre snap should be 0
           # after snap, during route should be 1,
           # anything greater than 1 is after the route is over
           play_group = cumsum(play_group)) %>%
    dplyr::filter(play_group == 1) %>%
    dplyr::select(-play_group)
  
}
