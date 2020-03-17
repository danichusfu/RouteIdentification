#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



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


#' Generate sample data from the  template curves
#'
#' @param cluster_controls cluster controls for the random curves
#' @param P Degree of the bezier curve
#' @param number_of_curves the number of curves you want to generate
#' @return The data describing the randomized curves from the clusters
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

#' Calculates updated beta given W, X, Y
#'
#' @param W weights
#' @param X X matrix
#' @param Y Y matrix
#' @return updated beta

calc_new_beta <- function(W, X, Y){
  #W <- diag(weights)
  Beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
  return(Beta_new)
}

#' Calculates updated sigma given W, X, Y, Beta
#'
#' @param W weights
#' @param X X matrix
#' @param Y Y matrix
#' @param Beta updated betas
#' @return updated sigma

calc_new_sigma <- function(W, X, Y, Beta){
  #W <- diag(weights)
  Sigma_new <- diag(diag(as.matrix(Matrix::t(Y - X %*% Beta) %*% W %*% (Y - X %*% Beta) / sum(W))))
}


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

#' plot cluster means given the results of the em
#'
#' @param em_results results of the em algorithm
#' @return plot of the cluster means

plot_cluster_means <- function(em_results){
  Beta <- em_results$Beta
  tibble::tibble(Beta = Beta) %>%
    dplyr::mutate(mean_curve = purrr::map(Beta, ~ bezier(t = seq(0, 1, by = 0.1), p = .) %>% tibble::as_tibble()),
           cluster = dplyr::row_number()) %>%
    dplyr::select(-Beta) %>%
    tidyr::unnest() %>%
    ggplot2::ggplot(aes(x = V2, y = V1, colour = factor(cluster))) +
    ggplot2::geom_path() +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x  = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y  = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank())
}

#' reorder clusters so that the biggest ones are numbered 1 and smallest n
#'
#' @param em_results results of em
#' @return reordered em_results

reorder_clusters <- function(em_results){


  # Save old Data
  Pik   <- em_results$Pik
  Beta  <- em_results$Beta
  Sigma <- em_results$Sigma
  Alpha <- em_results$Alpha
  K <- ncol(Pik)

  # reorder by depth
  reordered <-
    tibble::tibble(Beta, Sigma, Alpha) %>%
    dplyr::mutate(rank = dplyr::dense_rank(-Alpha),
                  row  = dplyr::row_number()) %>%
    dplyr::arrange(rank)

  # Reorder all of the variables
  row <-
    reordered %>%
    purrr::pull(row)

  Pik <- Pik[, row]
  colnames(Pik) <- 1:K


  Beta <-
    reordered %>%
    purrr::pull(Beta)

  Sigma <-
    reordered %>%
    purrr::pull(Sigma)

  Alpha <-
    reordered %>%
    purrr::pull(Alpha)

  em_results <-
    list("l_hood" = em_results$l_hood,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"   = Sigma,
         "Alpha"   = Alpha)

  return(em_results)

}

#' extracts the cluster means from em_results
#'
#' @param em_results results of the em algorithm
#' @return the extract clustered means

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
