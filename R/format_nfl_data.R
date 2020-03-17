

#' Read and format data from csv for NFL examples, assuming BDB or 903124 format (NextGenStats)
#' 
#' @param file_name Take a file name as it is required input to the csv parser

format_nfl_data <- function(file_name){
  
  # Load all the data
  routes_data <- 
    # create the tibble with the file names
    tibble(file_name = file_name) %>%
    # read the data in nested
    mutate(data = map(file_name, read_routes_from_csv)) %>%
    dplyr::select(-file_name) %>% 
    unnest(cols = c(data))
  
  # Transform our curves
  routes_data <-
    routes_data %>%
    mutate(row = row_number()) %>%
    mutate(data = pmap(list(data, team, direction_left, line_of_scrimmage), 
                       ~ cut_plays(..1) %>%
                         flip_field(., ..2, ..3, ..4)),
           n = map_dbl(data, nrow)) %>%
    filter(n >= 2) %>%
    # left side of field is TRUE
    mutate(data_same_sideline = purrr::map(data, 
                                           ~ mutate(., 
                                                    sof = 160/6 > first(y),
                                                    y = if_else(sof, 160/3 - y, y),
                                                    y = y - first(y)
                                           ) %>%
                                             dplyr::select(-sof)))  %>%
    arrange(row)
  
  # Formatting fix
  routes_data <-
    routes_data %>%
    ungroup() %>%
    select(-row)
  
  # nfl_em_results and cluster_route_map are objects that exist within this package and can be called freely
  routes_data <- routes_data %>%
    mutate(curve_num = row_number()) %>%
    unnest(cols = c(data_same_sideline)) 
  
  return(routes_data)
  
}
