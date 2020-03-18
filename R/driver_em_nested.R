

#' Analyse nested trajectory data
#' 
#' @param nest_traj Tibble containing the nested trajectory data with at least columns x, y
#' @param K Number of clusters to identify via the Expectation Maximization Algorithm
#' @return Tibble containing results of the EM algorithm
#' @export

driver_em_nested <- function(nest_traj, K){
  
  em_results <- nest_traj %>%
    tidyr::unnest(cols = c(x,y)) %>%
    cluster_trajectory_data(K = K)
  
  return(em_results)
}
