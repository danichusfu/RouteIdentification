

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
