

#' reorder clusters so that the biggest ones are numbered 1 and smallest n
#'
#' @param em_results results of em
#' @importFrom magrittr %>%
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
    dplyr::pull(row)
  
  Pik <- Pik[, row]
  colnames(Pik) <- 1:K
  
  
  Beta <-
    reordered %>%
    dplyr::pull(Beta)
  
  Sigma <-
    reordered %>%
    dplyr::pull(Sigma)
  
  Alpha <-
    reordered %>%
    dplyr::pull(Alpha)
  
  em_results <-
    list("l_hood" = em_results$l_hood,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"   = Sigma,
         "Alpha"   = Alpha)
  
  return(em_results)
  
}
