

#' plot cluster means given the results of the em
#'
#' @param em_results results of the em algorithm
#' @importFrom magrittr %>%
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
