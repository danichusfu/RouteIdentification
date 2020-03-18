

#' Read and format data from csv for NFL examples, assuming BDB or 903124 format (NextGenStats)
#' 
#' @param file_name Take a file name as it is required input to the csv parser
#' @param data_source either "ngs" or "903124" can we figure out how to make these the only two options?
#' @importFrom magrittr %>%
#' @return A tibble containing the original routes data formatted for use with functions in this package
#' @export

format_nfl_data <- function(file_name, data_source = "ngs"){
  
  
  # different functions depending on data source
  # might need error if neither source is present
  if (data_source == "ngs"){
    read_routes_from_csv <- read_routes_from_csv
    flip_field <- flip_field
  } else{
    read_routes_from_csv <- read_routes_from_903124
    flip_field <- flip_field_903124
  }
  
  
  
  # Load all the data
  routes_data <- 
    # create the tibble with the file names
    tibble::tibble(file_name = file_name) %>%
    # read the data in nested
    dplyr::mutate(data = purrr::map(file_name, read_routes_from_csv)) %>%
    dplyr::select(-file_name) %>% 
    tidyr::unnest(cols = c(data))
  
  # Transform our curves
  routes_data <-
    routes_data %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(data = purrr::pmap(list(data, team, direction_left, line_of_scrimmage), 
                       ~ cut_plays(..1) %>%
                         flip_field(., ..2, ..3, ..4)),
           n = purrr::map_dbl(data, nrow)) %>%
    dplyr::filter(n >= 2) %>%
    # left side of field is TRUE
    dplyr::mutate(data_same_sideline = purrr::map(data, 
                                           ~ dplyr::mutate(., 
                                                    sof = 160/6 > first(y),
                                                    y = if_else(sof, 160/3 - y, y),
                                                    y = y - first(y)
                                           ) %>%
                                             dplyr::select(-sof)))  %>%
    dplyr::arrange(row)
  
  # Formatting fix
  routes_data <-
    routes_data %>%
    dplyr::ungroup() %>%
    dplyr::select(-row)
  
  # nfl_em_results and cluster_route_map are objects that exist within this package and can be called freely
  routes_data <- routes_data %>%
    dplyr::mutate(curve_num = dplyr::row_number()) %>%
    tidyr::unnest(cols = c(data_same_sideline)) 
  
  return(routes_data)
  
}
