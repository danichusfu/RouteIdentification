

#' Assign clusters to never before seen data
#'
#' @param new_trajectory_data the new data to cluster
#' @param em_results the results of the previous clustering
#' @return The clusters for the new data
#' @export

fit_new_data <- function(new_trajectory_data, em_results){
  
  Alpha <- em_results$Alpha
  Beta <- em_results$Beta
  Sigma <- em_results$Sigma
  
  
  new_piks <-
    new_trajectory_data %>%
    tidyr::nest(data = c(x, y)) %>%
    dplyr::mutate(piks = map(data, ~ new_data_probs(., Alpha, Beta, Sigma)))

  new_data_fitted <-
    new_piks %>%
    dplyr::mutate(cluster_assigned = map_dbl(piks, which.max)) %>%
    dplyr::select(-piks)

  return(new_data_fitted)
}


