

#' Generate random controls for different template curves
#'
#' @param number_of_clusters number of template curves
#' @param P Degree of the bezier curve
#' @return The randomized cluster controls
generate_random_cluster_controls <- function(number_of_clusters = 5, P = 3){
  cluster_controls <-
    tibble::tibble(cluster_num = 1:number_of_clusters) %>%
    dplyr::mutate(Beta = purrr::map(cluster_num, ~ tibble::tibble(x = c(5, round(10 * runif(P))),
                                                                  y = c(0, round(10 * runif(P))))),
                  sigma_2 = list(tibble::tibble(x = 0.001,
                                                y = 0.001)))
  
  return(cluster_controls)
}
