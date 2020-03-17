

#' Analyse nested trajectory data
#' 
#' @param nest_traj Tibble containing the nested trajectory data with at least columns x, y
#' @param K Number of clusters to identify via the Expectation Maximization Algorithm

driver_em_nested <- function(nest_traj, K){
  
  em_results <- nest_traj %>%
    unnest(cols = c(x,y)) %>%
    cluster_trajectory_data(K = K)
  
  return(em_results)
}