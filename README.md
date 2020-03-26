
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RouteIdentification

<!-- badges: start -->

<!-- badges: end -->

The goal of RouteIdentification is to clustery trajectory data sets like
those found in sports analytics tracking data. This work was developed
as part of the first Big Data Bowl and the methodology is written about
in
[JQAS](https://www.degruyter.com/view/journals/jqas/ahead-of-print/article-10.1515-jqas-2019-0047/article-10.1515-jqas-2019-0047.xml). You may alse find the article on [arXiv](https://arxiv.org/abs/1908.02423).

## Installation

<!--You can install the released version of RouteIdentification from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RouteIdentification")
```
 -->

See this great link for help getting github auth to install the package
from the private repo. [This is the great
link.](https://happygitwithr.com/github-pat.html)

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("danichusfu/RouteIdentification")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# devtools::install_github(repo = "danichusfu/RouteIdentification")

library(RouteIdentification)
#> Warning: replacing previous import 'magrittr::set_names' by 'purrr::set_names'
#> when loading 'RouteIdentification'
#> Warning: replacing previous import 'magrittr::extract' by 'tidyr::extract' when
#> loading 'RouteIdentification'
library(tidyverse)
#> -- Attaching packages -------------------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.2.1     v purrr   0.3.3
#> v tibble  2.1.3     v dplyr   0.8.4
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.4.0
#> -- Conflicts ----------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

# Generate data
nested_trajectory_data <- rand_centred_curves(n_clust = 3, n_curves = 20)

# Apply EM algorithm, either to generated data or appropriately formatted data
em_results <- driver_em_nested(nested_trajectory_data, K = 3)
#> 0 sec elapsed
#> [1] 1
#> [1] "e_step time"
#> 0.66 sec elapsed
#> [1] -Inf
#> [1] "m_step time"
#> 0.06 sec elapsed
#> [1] 2
#> [1] "e_step time"
#> 0.61 sec elapsed
#> [1] 3422.616
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 3
#> [1] "e_step time"
#> 0.64 sec elapsed
#> 2 sec elapsed

# Grab the cluster means
cluster_means <- extract_cluster_means(em_results)

# Identify cluster assignments
cluster_assignments <- identify_clusters(nested_trajectory_data, em_results)

# Count cluster assignments
cluster_assignments %>%
  count(cluster, pred_cluster)
#> # A tibble: 3 x 3
#>   cluster pred_cluster     n
#>     <dbl>        <dbl> <int>
#> 1       1            3     6
#> 2       2            1     7
#> 3       3            2     7

# Plot clusters assigments by assigned cluster mean
cluster_assignments %>%
  plot_curve_assign()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

# Simple plot of just the cluster means, no other curves
cluster_means %>%
  ggplot(aes(x = V1, y = V2, colour = factor(cluster))) +
  geom_path() +
  facet_wrap(~ cluster)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

``` r

# Generate new data from the globally assigned cluster_controls, generated in rand_centred_curves()
new_nested_trajectory_data <-
  generate_sample_data(cluster_controls) %>%
  select(curve_i, x, y, cluster = cluster_num)

new_trajectory_data <- new_nested_trajectory_data %>% unnest(cols = c(x, y))

# Fit the new data to the Expectation Maximization results (naming to be updated)
new_data_fit <- fit_new_data(new_trajectory_data, em_results)

# Tabulate assignments
new_data_fit %>%
  count(cluster, cluster_assigned)
#> # A tibble: 3 x 3
#>   cluster cluster_assigned     n
#>     <dbl>            <dbl> <int>
#> 1       1                3    48
#> 2       2                1    47
#> 3       3                2    25
```

## Now with NFL sample data

``` r
# Use online sample data from big data bowl

# list all the files
#tracking_files <- list.files(path = "Data/", pattern = "tracking_.*\\.csv")

# Parse NFL data based on file input
nfl_bdb_sample <- format_nfl_data(file_name = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv",
                                  data_source = "ngs")

fitted_clusters <- nfl_bdb_sample %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))

# Overview of the assigned routes
nfl_bdb_sample %>%
  nest(cols = -c(gameId, playId, displayName)) %>%
  select(gameId, playId, displayName) %>%
  bind_cols(fitted_clusters %>% select(route_name))
#> # A tibble: 394 x 4
#>        gameId playId displayName    route_name
#>         <dbl>  <dbl> <chr>          <chr>     
#>  1 2017090700     68 Rob Gronkowski dig/over  
#>  2 2017090700     68 Chris Hogan    go/seam   
#>  3 2017090700     68 Dwayne Allen   corner    
#>  4 2017090700     68 Rex Burkhead   flat      
#>  5 2017090700     68 Brandin Cooks  go/seam   
#>  6 2017090700     94 Danny Amendola corner    
#>  7 2017090700     94 Rob Gronkowski flat      
#>  8 2017090700     94 Chris Hogan    dig/over  
#>  9 2017090700     94 Rex Burkhead   flat      
#> 10 2017090700     94 Brandin Cooks  deep_out  
#> # ... with 384 more rows
```

## Now with the higlight data from 903124

``` r
## Another NFL example: NextGenStats Scraped Data (compliments to @903124S)

nfl_ngs_sample <- format_nfl_data(file_name = "https://raw.githubusercontent.com/danichusfu/NFL_Highlight_Tracking/master/Highlight_19_post.csv",
                                  data_source = "903124")
#> Warning: Missing column names filled in: 'X1' [1]

fitted_clusters <- nfl_ngs_sample %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))

# Overview of the assigned routes
nfl_ngs_sample %>%
  nest(cols = -c(gameId, playId, displayName)) %>%
  select(gameId, playId, displayName) %>%
  bind_cols(fitted_clusters %>% select(route_name))
#> # A tibble: 124 x 4
#>        gameId playId displayName     route_name
#>         <dbl>  <dbl> <chr>           <chr>     
#>  1 2020010501   2688 Luke Willson    blocking  
#>  2 2020010501   2688 Tyler Lockett   flat      
#>  3 2020010501   2688 David Moore     post      
#>  4 2020010501   2688 Travis Homer    flat      
#>  5 2020010501   2688 D.K. Metcalf    go/seam   
#>  6 2020010400   3187 DeAndre Hopkins corner    
#>  7 2020010400   3187 Kenny Stills    dig/over  
#>  8 2020010400   3187 Darren Fells    blocking  
#>  9 2020010400   3187 Carlos Hyde     blocking  
#> 10 2020010400   3187 DeAndre Carter  flat      
#> # ... with 114 more rows
```
