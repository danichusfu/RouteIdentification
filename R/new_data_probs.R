
#' calculate the probability of assignment for new data
#'
#' @param routes_data the new trajectory data
#' @param Alpha The alpha from the emclustering
#' @param Beta the beta from the em clustering
#' @param Sigma  the sigma from the em clustering
#' @return probabilities for each new cluster

new_data_probs <- function(routes_data, Alpha, Beta, Sigma){
  
  
  P <- nrow(Beta[[1]]) - 1
  
  
  # create data for em algorithm
  routes_data <-
    routes_data %>%
    tidyr::nest(data = c(x, y)) %>%
    dplyr::mutate(curve_i = row_number()) %>%
    dplyr::mutate(n_i = purrr::map_dbl(data, nrow),
                  t_i = purrr::map(n_i, ~ tibble::tibble(t = (1:. - 1)/(.-1))),
                  T_i = purrr::map(t_i, ~ dplyr::mutate(., p = list(0:P)) %>%
                                     tidyr::unnest(cols = c(p)) %>%
                                     dplyr::mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                                     tidyr::spread(p, D_p_of_t, sep = "_") %>%
                                     dplyr::select(-t))) %>%
    dplyr::select(data, curve_i, n_i, t_i, T_i) %>%
    dplyr::arrange(curve_i) %>%
    dplyr::ungroup()
  
  
  # create X matrix
  X <-
    routes_data %>%
    dplyr::select(T_i) %>%
    tidyr::unnest(c(T_i)) %>%
    Matrix::as.matrix()
  
  # create Y matrix
  Y <-
    routes_data %>%
    dplyr::select(data) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::select(x, y) %>%
    Matrix::as.matrix()
  
  SEQ <-
    routes_data %>%
    dplyr::select(n_i) %>%
    dplyr::mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()
  
  INDEX <-
    routes_data %>%
    dplyr::select(curve_i, t_i) %>%
    tidyr::unnest(cols = c(t_i)) %>%
    dplyr::select(curve_i)
  
  n <- length(SEQ)
  N <- SEQ[n]
  curve_lengths <- routes_data %>% dplyr::select(n_i)
  
  
  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = purrr::pmap_dbl(list(x, y, x1, y1), ~ dmvnorm(c(..1, ..2),
                                                                     c(..3, ..4),
                                                                     Sigma))) %>%
      dplyr::bind_cols(tibble::as_tibble(INDEX))
  }
  
  
  data_Piik <-
    dplyr::tibble(Beta, Sigma) %>%
    dplyr::mutate(k = dplyr::row_number()) %>%
    dplyr::mutate(X_Beta = purrr::map(Beta,
                                      ~ Matrix::as.matrix(X) %*% .x %>%
                                        tibble::as_tibble() %>%
                                        dplyr::bind_cols(tibble::as_tibble(Matrix::as.matrix(Y))))) %>%
    dplyr::mutate(Piik = purrr::map2(X_Beta, Sigma, calc_Piik)) %>%
    dplyr::select(k, Piik) %>%
    tidyr::unnest(cols = c(Piik))
  
  scale_m <-
    data_Piik %>%
    dplyr::ungroup() %>%
    dplyr::summarise(mean = mean(Piik)) %>%
    dplyr::pull(mean)
  
  Pik <-
    data_Piik %>%
    dplyr::mutate(Piik = Piik/scale_m) %>%
    dplyr::group_by(curve_i, k) %>%
    dplyr::summarise(Pik = prod(Piik))  %>%
    tidyr::spread(k, Pik) %>%
    dplyr::ungroup() %>%
    dplyr::select(-curve_i) %>%
    Matrix::as.matrix()
  
  Pik <- Pik * Alpha
  
  #############################################################################
  
  # Calculate Log Likelihood
  
  # Calculate Probability of data over all clusters
  s <- rowSums(Pik)
  
  # Since we're not on the log scale we might get 0
  if(any(s == 0)){
    # replace 0 with the smallest number possible
    # then weight by the alphas
    Pik[s == 0, ] <- .Machine$double.xmin * Alpha
    # recalculate the probability of observing this data over all clusters
    s <- rowSums(Pik)
  }
  
  
  # Calculate the Pi_ik
  Pik <- Pik/s
  
  return(Pik)
}