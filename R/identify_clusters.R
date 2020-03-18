

#' Identify the curves assigned to each cluster via Expectation Maximization Algorithm
#' 
#' @param nest_traj Tibble containing the nested trajectory data with at least columns x, y
#' @param em_results The output of driver_em_nested()
#' @importFrom magrittr %>%
#' @return tibble containing the original input and the assigned clusters from the EM algorithm
#' @export

identify_clusters <- function(nest_traj, em_results){
  
  id_table <- nest_traj %>%
    dplyr::bind_cols(as_tibble(em_results$Pik)) %>%
    dplyr::mutate(curve_i = dplyr::row_number()) %>%
    tidyr::pivot_longer(names_to = "pred_cluster", values_to = "prob", dplyr::matches("\\d")) %>%
    dplyr::mutate(pred_cluster = readr::parse_number(pred_cluster)) %>%
    dplyr::group_by(curve_i) %>%
    dplyr::filter(prob == max(prob)) %>%
    dplyr::ungroup() 
  
  return(id_table)
}
