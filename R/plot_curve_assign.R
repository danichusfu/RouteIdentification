

#' Generate a facet wrapped ggplot2 object of the curves and their matched cluster mean
#' 
#' @param nest_traj_with_em The output of identify_clusters, contains the raw tracking data and curve assignments

plot_curve_assign <- function(nest_traj_with_em){
  
  p1 <- nest_traj_with_em %>%
    unnest(cols = c(x,y)) %>%
    ggplot(aes(x = x, y = y, group = curve_i, colour = factor(cluster))) +
    geom_path() +
    facet_wrap(~ pred_cluster)
  
  return(p1)
}