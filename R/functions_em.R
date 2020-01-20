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
    mutate(Beta = map(cluster_num, ~ tibble(x = c(5, round(10 * runif(P))),
                                            y = c(0, round(10 * runif(P))))),
           sigma_2 = list(tibble(x = 0.001,
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
    tibble(curve_i = 1:number_of_curves) %>%
    mutate(cluster_num = map_dbl(curve_i, ~ ceiling(nrow(cluster_controls) * runif(1))),
           n_i = map_dbl(curve_i, ~ round(20 + runif(1, 0.5, 1) * 30)),
           t_i = map(n_i, ~ tibble(t = (1:. - 1)/(.-1))),
           T_i = map(t_i, ~ mutate(., p = list(0:P)) %>%
                       unnest(cols = c(p)) %>%
                       mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                       spread(p, D_p_of_t, sep = "_") %>%
                       select(-t))) %>%
    left_join(cluster_controls, by = "cluster_num") %>%
    mutate(x = pmap(list(T_i, Beta, sigma_2), ~ tibble(x = as.vector(as.matrix(..1) %*% as.matrix(..2[, 1]) +
                                                                       MASS::mvrnorm(n = 1,
                                                                                     mu = rep(0, nrow(..1)),
                                                                                     Sigma = diag(..3,
                                                                                                  nrow = nrow(..1),
                                                                                                  ncol = nrow(..1)))))),
           y = pmap(list(T_i, Beta, sigma_2), ~ tibble(y = as.vector(as.matrix(..1) %*% as.matrix(..2[, 2]) +
                                                                       MASS::mvrnorm(n = 1,
                                                                                     mu = rep(0, nrow(..1)),
                                                                                     Sigma = diag(..3,
                                                                                                  nrow = nrow(..1),
                                                                                                  ncol = nrow(..1)))))))

  data <- data %>% select(curve_i, cluster_num, n_i, t_i, T_i, x, y)

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
    prepared_trajectory_data %>%
    select(T_i) %>%
    unnest(cols = c(T_i)) %>%
    Matrix::as.matrix()

  # create Y matrix
  Y <-
    prepared_trajectory_data %>%
    select(data) %>%
    unnest(cols = data) %>%
    Matrix::as.matrix()

  SEQ <-
    prepared_trajectory_data %>%
    select(n_i) %>%
    mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()

  INDEX <-
    prepared_trajectory_data %>%
    select(curve_i, t_i) %>%
    unnest(cols = c(t_i)) %>%
    select(curve_i)

  ##################### INIT

  kmean_data <-
    prepared_trajectory_data %>%
    transmute(ends = map(data, ~ filter(., row_number() == max(row_number())) %>% select(x, y))) %>%
    unnest(cols = c(ends))


  kmeans_results <- kmeans(kmean_data, centers = K, iter.max = 100)

  kmean_data %>%
    mutate(cluster = kmeans_results$cluster) %>%
    ggplot(aes(x = x, y = y, colour = factor(cluster))) +
    geom_point() +
    theme_bw()

  init_clusters <-
    prepared_trajectory_data %>%
    mutate(cluster = kmeans_results$cluster) %>%
    unnest(data) %>%
    select(cluster)


  Alpha <-
    init_clusters %>%
    group_by(cluster) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n)) %>%
    pull(prop)

  c <- init_clusters %>% pull(cluster)


  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = pmap_dbl(list(x, y, x1, y1), ~ dmvnorm(c(..1, ..2),
                                                              c(..3, ..4),
                                                              Sigma))) %>%
      bind_cols(as_tibble(INDEX))
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
  curve_lengths <- prepared_trajectory_data %>% select(n_i)

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
      tibble(Beta, Sigma) %>%
      mutate(k = row_number()) %>%
      #partition() %>%
      mutate(X_Beta = map(Beta,
                          ~ Matrix::as.matrix(X) %*% .x %>%
                            as_tibble() %>%
                            bind_cols(as_tibble(Matrix::as.matrix(Y))))) %>%
      mutate(Piik = map2(X_Beta, Sigma, calc_Piik)) %>%
      select(k, Piik) %>%
      #collect() %>%
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
      as_tibble() %>%
      mutate(curve_i = row_number()) %>%
      right_join(INDEX, by = "curve_i") %>%
      select(matches("\\d")) %>%
      as.list()

    param_updates <-
      tibble(k = 1:K, weights = weights) %>%
      mutate(weights = map(weights, ~ Matrix::Diagonal(x = .))) %>%
      #partition() %>%
      mutate(Beta_new  = map(weights, ~ calc_new_beta(., X, Y)),
             Sigma_new = map2(weights, Beta_new, ~ calc_new_sigma(.x, X, Y, .y)),
             Beta_new = map(Beta_new, as.matrix)) %>%
      #collect() %>%
      arrange(k)

    Beta <- param_updates %>% pull(Beta_new)
    Sigma <- param_updates %>% pull(Sigma_new)

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
  tibble(Beta = Beta) %>%
    mutate(mean_curve = map(Beta, ~ bezier(t = seq(0, 1, by = 0.1), p = .) %>% as_tibble()),
           cluster = row_number()) %>%
    select(-Beta) %>%
    unnest() %>%
    ggplot(aes(x = V2, y = V1, colour = factor(cluster))) +
    geom_path() +
    coord_fixed() +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
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
    tibble(Beta, Sigma, Alpha) %>%
    mutate(rank = dense_rank(-Alpha),
           row  = row_number()) %>%
    arrange(rank)

  # Reorder all of the variables
  row <-
    reordered %>%
    pull(row)

  Pik <- Pik[, row]
  colnames(Pik) <- 1:K


  Beta <-
    reordered %>%
    pull(Beta)

  Sigma <-
    reordered %>%
    pull(Sigma)

  Alpha <-
    reordered %>%
    pull(Alpha)

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
    tibble(Beta = Beta) %>%
    mutate(mean_curve = map(Beta, ~ bezier(t = seq(0, 1, by = 0.05), p = .) %>% as_tibble()),
           cluster = row_number()) %>%
    select(-Beta) %>%
    unnest(cols = c(mean_curve))
  return(cluster_means)
}
