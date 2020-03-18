

#' Generate a facet wrapped ggplot2 object of the curves and their matched cluster mean
#' 
#' @param nest_traj_with_em The output of identify_clusters, contains the raw tracking data and curve assignments
#' @return ggplot2 object of the curves and their matched cluster mean
#' @importFrom magrittr %>%
#' @export

plot_curve_assign <- function(nest_traj_with_em){
  
  p1 <- nest_traj_with_em %>%
    tidyr::unnest(cols = c(x,y)) %>%
    ggplot2::ggplot(aes(x = x, y = y, group = curve_i, colour = factor(cluster))) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ pred_cluster)
  
  return(p1)
}
