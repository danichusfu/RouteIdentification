

#' Generate sample data from the  template curves
#'
#' @param cluster_controls cluster controls for the random curves
#' @param P Degree of the bezier curve
#' @param number_of_curves the number of curves you want to generate
#' @importFrom magrittr %>%
#' @return The data describing the randomized curves from the clusters
#' @export

generate_sample_data <- function(cluster_controls, P = 3, number_of_curves = 120){
  
  
  data <-
    tibble::tibble(curve_i = 1:number_of_curves) %>%
    dplyr::mutate(cluster_num = map_dbl(curve_i, ~ ceiling(nrow(cluster_controls) * runif(1))),
                  n_i = purrr::map_dbl(curve_i, ~ round(20 + runif(1, 0.5, 1) * 30)),
                  t_i = purrr::map(n_i, ~ tibble(t = (1:. - 1)/(.-1))),
                  T_i = purrr::map(t_i, ~ mutate(., p = list(0:P)) %>%
                                     tidyr::unnest(cols = c(p)) %>%
                                     dplyr::mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                                     tidyr::spread(p, D_p_of_t, sep = "_") %>%
                                     dplyr::select(-t))) %>%
    dplyr::left_join(cluster_controls, by = "cluster_num") %>%
    dplyr::mutate(x = purrr::pmap(list(T_i, Beta, sigma_2), 
                                  ~ tibble(x = as.vector(as.matrix(..1) %*% as.matrix(..2[, 1]) +
                                                           MASS::mvrnorm(n = 1,
                                                                         mu = rep(0, nrow(..1)),
                                                                         Sigma = diag(..3,
                                                                                      nrow = nrow(..1),
                                                                                      ncol = nrow(..1)))))),
                  y = purrr::pmap(list(T_i, Beta, sigma_2), ~ tibble(y = as.vector(as.matrix(..1) %*% as.matrix(..2[, 2]) +
                                                                                     MASS::mvrnorm(n = 1,
                                                                                                   mu = rep(0, nrow(..1)),
                                                                                                   Sigma = diag(..3,
                                                                                                                nrow = nrow(..1),
                                                                                                                ncol = nrow(..1)))))))
  
  data <- data %>% dplyr::select(curve_i, cluster_num, n_i, t_i, T_i, x, y)
  
  return(data)
}
