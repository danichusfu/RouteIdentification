

#' Identify the curves assigned to each cluster via Expectation Maximization Algorithm
#' 
#' @param nest_traj Tibble containing the nested trajectory data with at least columns x, y
#' @param em_results The output of driver_em_nested()
#' 

identify_clusters <- function(nest_traj, em_results){
  
  id_table <- nest_traj %>%
    bind_cols(as_tibble(em_results$Pik)) %>%
    mutate(curve_i = row_number()) %>%
    pivot_longer(names_to = "pred_cluster", values_to = "prob", matches("\\d")) %>%
    mutate(pred_cluster = parse_number(pred_cluster)) %>%
    group_by(curve_i) %>%
    filter(prob == max(prob)) %>%
    ungroup() 
  
  return(id_table)
}