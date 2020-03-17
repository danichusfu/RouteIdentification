

#' Clusters trajectory data
#'
#' @param trajectory_data data to cluster
#' @param P degree of bezier curve
#' @param K number of clusters
#' @param niter number of iteration to run it for
#' @return results of the clustering

cluster_trajectory_data <- function(trajectory_data, P = 3, K = 5, niter = 20){
  
  # create data for em algorithm
  prepared_trajectory_data <-
    trajectory_data %>%
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
    ungroup()
  
  # create X matrix
  X <-
    prepared_trajectory_data %>%
    dplyr::select(T_i) %>%
    tidyr::unnest(cols = c(T_i)) %>%
    Matrix::as.matrix()
  
  # create Y matrix
  Y <-
    prepared_trajectory_data %>%
    dplyr::select(data) %>%
    tidyr::unnest(cols = data) %>%
    Matrix::as.matrix()
  
  SEQ <-
    prepared_trajectory_data %>%
    dplyr::select(n_i) %>%
    dplyr::mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()
  
  INDEX <-
    prepared_trajectory_data %>%
    dplyr::select(curve_i, t_i) %>%
    tidyr::unnest(cols = c(t_i)) %>%
    dplyr::select(curve_i)
  
  ##################### INIT
  
  kmean_data <-
    prepared_trajectory_data %>%
    dplyr::transmute(ends = purrr::map(data, ~ filter(., row_number() == max(row_number())) %>% dplyr::select(x, y))) %>%
    tidyr::unnest(cols = c(ends))
  
  
  kmeans_results <- kmeans(kmean_data, centers = K, iter.max = 100)
  
  kmean_data %>%
    dplyr::mutate(cluster = kmeans_results$cluster) %>%
    ggplot2::ggplot(aes(x = x, y = y, colour = factor(cluster))) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()
  
  init_clusters <-
    prepared_trajectory_data %>%
    dplyr::mutate(cluster = kmeans_results$cluster) %>%
    tidyr::unnest(data) %>%
    dplyr::select(cluster)
  
  
  Alpha <-
    init_clusters %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(prop = n/sum(n)) %>%
    purrr::pull(prop)
  
  c <- init_clusters %>% purrr::pull(cluster)
  
  
  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = purrr::pmap_dbl(list(x, y, x1, y1), ~ dmvnorm(c(..1, ..2),
                                                                     c(..3, ..4),
                                                                     Sigma))) %>%
      dplyr::bind_cols(as_tibble(INDEX))
  }
  
  Beta <- list()
  Sigma <- list()
  tic()
  for(k in 1:K){
    Beta[[k]] <- solve(t(X[k == c, ]) %*% X[k == c, ]) %*% t(X[k == c, ]) %*% Y[k == c, ]
    Sigma[[k]] <- diag(diag(t(Y[k == c, ] - X[k == c, ] %*% Beta[[k]]) %*% (Y[k == c, ] - X[k == c, ] %*% Beta[[k]])/ nrow(X[k == c, ])))
  }
  toc()
  
  n <- length(SEQ)
  N <- SEQ[n]
  curve_lengths <- prepared_trajectory_data %>% dplyr::select(n_i)
  
  #################################################################################
  l_hood <- -Inf
  tic()
  
  
  for(i in 1:niter){
    print(i)
    
    #############################################################################
    # Expectation Step
    print("e_step time")
    tic()
    
    data_Piik <-
      tibble::tibble(Beta, Sigma) %>%
      dplyr::mutate(k = row_number()) %>%
      #partition() %>%
      dplyr::mutate(X_Beta = purrr::map(Beta,
                                        ~ Matrix::as.matrix(X) %*% .x %>%
                                          tibble::as_tibble() %>%
                                          dplyr::bind_cols(as_tibble(Matrix::as.matrix(Y))))) %>%
      dplyr::mutate(Piik = purrr::map2(X_Beta, Sigma, calc_Piik)) %>%
      dplyr::select(k, Piik) %>%
      #collect() %>%
      tidyr::unnest(cols = c(Piik))
    
    scale_m <-
      data_Piik %>%
      dplyr::ungroup() %>%
      dplyr::summarise(mean = mean(Piik)) %>%
      purrr::pull(mean)
    
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
    
    
    
    toc()
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
    
    # Now calculate the new log likelihood
    l_hood_new <- sum(log(s)) + N * log(scale_m)
    
    # If we've reached our tolerance stop the loop
    if(abs(l_hood - l_hood_new) < 1e-6){
      break
    }
    
    # For monitoring
    print(l_hood)
    # overwrite the old log likelihood
    l_hood <- l_hood_new
    
    # Calculate the Pi_ik
    Pik <- Pik/s
    
    # Perform Maximization Step
    
    print("m_step time")
    tic()
    #############################################################################
    Alpha <- colSums(Pik) / n
    
    
    weights <-
      Pik %>%
      tibble::as_tibble() %>%
      dplyr::mutate(curve_i = row_number()) %>%
      dplyr::right_join(INDEX, by = "curve_i") %>%
      dplyr::select(matches("\\d")) %>%
      as.list()
    
    param_updates <-
      tibble::tibble(k = 1:K, weights = weights) %>%
      dplyr::mutate(weights = purrr::map(weights, ~ Matrix::Diagonal(x = .))) %>%
      #partition() %>%
      dplyr::mutate(Beta_new  = purrr::map(weights, ~ calc_new_beta(., X, Y)),
                    Sigma_new = purrr::map2(weights, Beta_new, ~ calc_new_sigma(.x, X, Y, .y)),
                    Beta_new  = purrr::map(Beta_new, as.matrix)) %>%
      #collect() %>%
      dplyr::arrange(k)
    
    Beta <- param_updates %>% purrr::pull(Beta_new)
    Sigma <- param_updates %>% purrr::pull(Sigma_new)
    
    #############################################################################
    toc()
    
    
    
  }
  toc()
  
  em_results <-
    list("l_hood" = l_hood_new,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"  = Sigma,
         "Alpha"  = Alpha)
  
  em_results <-
    em_results %>%
    reorder_clusters()
  
  return(em_results)
}
