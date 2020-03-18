

#' extracts the cluster means from em_results
#'
#' @param em_results results of the em algorithm
#' @return the extract clustered means
#' @export

extract_cluster_means <- function(em_results){
  Beta <- em_results$Beta
  cluster_means <-
    tibble::tibble(Beta = Beta) %>%
    dplyr::mutate(mean_curve = purrr::map(Beta, 
                                          ~ bezier(t = seq(0, 1, by = 0.05), p = .) %>% 
                                            tibble::as_tibble()),
                  cluster = dplyr::row_number()) %>%
    dplyr::select(-Beta) %>%
    tidyr::unnest(cols = c(mean_curve))
  return(cluster_means)
}
