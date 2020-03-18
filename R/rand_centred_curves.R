

#' Create a number of curves from a provided number of clusters
#' 
#' @param n_clust the number of clusters to define randomly
#' @param n_curves the number of curves to define about the clusters randomly
#' @return Tibble containing randomly generated data about the provided number of cluster means
#' @export

rand_centred_curves <- function(n_clust, n_curves){
  
  # Global assignment such that this random creation can be called later to generate new data
  # Enforces a workflow of one set of randomly generated clusters at a time
  cluster_controls <<- generate_random_cluster_controls(number_of_clusters = n_clust)
  
  nested_trajectory_data <-
    generate_sample_data(cluster_controls, number_of_curves = n_curves) %>%
    select(curve_i, x, y, cluster = cluster_num)
  
  return(nested_trajectory_data)
}
