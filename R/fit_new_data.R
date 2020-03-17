

#' Assign clusters to never before seen data
#'
#' @param new_trajectory_data the new data to cluster
#' @param em_results the results of the previous clustering
#' @return The clusters for the never before seen data
fit_new_data <- function(new_trajectory_data, em_results){
  
  Alpha <- em_results$Alpha
  Beta <- em_results$Beta
  Sigma <- em_results$Sigma
  
  
  new_piks <-
    new_trajectory_data %>%
    nest(data = c(x, y)) %>%
    mutate(piks = map(data, ~ new_data_probs(., Alpha, Beta, Sigma)))
  
  new_data_fitted <-
    new_piks %>%
    mutate(cluster_assigned = map_dbl(piks, which.max)) %>%
    select(-piks)
  
  return(new_data_fitted)
}
