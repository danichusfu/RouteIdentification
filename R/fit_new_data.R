

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
    nest(data = c(x, y)) %>%
    mutate(curve_i = row_number()) %>%
    mutate(n_i = map_dbl(data, nrow),
           t_i = map(n_i, ~ tibble(t = (1:. - 1)/(.-1))),
           T_i = map(t_i, ~ mutate(., p = list(0:P)) %>%
                       unnest(cols = c(p)) %>%
                       mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                       spread(p, D_p_of_t, sep = "_") %>%
                       select(-t))) %>%
    select(data, curve_i, n_i, t_i, T_i) %>%
    arrange(curve_i) %>%
    ungroup()


  # create X matrix
  X <-
    routes_data %>%
    select(T_i) %>%
    unnest(c(T_i)) %>%
    Matrix::as.matrix()

  # create Y matrix
  Y <-
    routes_data %>%
    select(data) %>%
    unnest(cols = c(data)) %>%
    select(x, y) %>%
    Matrix::as.matrix()

  SEQ <-
    routes_data %>%
    select(n_i) %>%
    mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()

  INDEX <-
    routes_data %>%
    select(curve_i, t_i) %>%
    unnest(cols = c(t_i)) %>%
    select(curve_i)

  n <- length(SEQ)
  N <- SEQ[n]
  curve_lengths <- routes_data %>% select(n_i)


  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = pmap_dbl(list(x, y, x1, y1), ~ dmvnorm(c(..1, ..2),
                                                              c(..3, ..4),
                                                              Sigma))) %>%
      bind_cols(as_tibble(INDEX))
  }


  data_Piik <-
    tibble(Beta, Sigma) %>%
    mutate(k = row_number()) %>%
    mutate(X_Beta = map(Beta,
                        ~ Matrix::as.matrix(X) %*% .x %>%
                          as_tibble() %>%
                          bind_cols(as_tibble(Matrix::as.matrix(Y))))) %>%
    mutate(Piik = map2(X_Beta, Sigma, calc_Piik)) %>%
    select(k, Piik) %>%
    unnest(cols = c(Piik))

  scale_m <-
    data_Piik %>%
    ungroup() %>%
    summarise(mean = mean(Piik)) %>%
    pull(mean)

  Pik <-
    data_Piik %>%
    mutate(Piik = Piik/scale_m) %>%
    group_by(curve_i, k) %>%
    summarise(Pik = prod(Piik))  %>%
    spread(k, Pik) %>%
    ungroup() %>%
    select(-curve_i) %>%
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
