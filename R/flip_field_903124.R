

#' flip field about the split line
#'
#' @param data routes from the tracking data
#' @param team placeholder for now, need to adjust functionality with that of flip_field.R
#' @param left the team the player plays on
#' @param line_of_scrimmage what the line of scrimmage is for tat play
#' @importFrom magrittr %>%
#' @return return te data that has been flipped about the split line
#' @export

flip_field_903124 <- function(data, team, left, line_of_scrimmage){
  dplyr::mutate(data,
         # flip the field to try and get all routes in the same direction
         dir = left,
         x   = if_else(dir, 120 - x, x),
         y   = if_else(dir, 160/3 - y, y),
         # make the line of scrimmage start at the same place for every route.
         x   = if_else(dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) %>%
    tidyr::drop_na(x)
}
