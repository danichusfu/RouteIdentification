
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RouteIdentification

<!-- badges: start -->

<!-- badges: end -->

The goal of RouteIdentification is to
…

## Installation

<!--You can install the released version of RouteIdentification from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RouteIdentification")
```
 -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("danichusfu/RouteIdentification")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# devtools::install_github(repo = "danichusfu/RouteIdentification")

library(RouteIdentification)
#> Loading required package: ggplot2
#> Loading required package: purrr
#> Loading required package: tibble
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyr
#> Loading required package: stringr
#> Loading required package: readr
#> Loading required package: forcats
#> Loading required package: magrittr
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> Loading required package: tictoc
#> Loading required package: emdbook
#> Loading required package: bezier
#> Warning: replacing previous import 'tidyr::extract' by 'magrittr::extract' when
#> loading 'RouteIdentification'
#> Warning: replacing previous import 'purrr::set_names' by 'magrittr::set_names'
#> when loading 'RouteIdentification'

cluster_controls <- generate_random_cluster_controls(number_of_clusters = 3)

nested_trajectory_data <-
  generate_sample_data(cluster_controls, number_of_curves = 20) %>%
  select(curve_i, x, y, cluster = cluster_num)


em_results <-
  nested_trajectory_data %>%
  unnest(cols = c(x, y)) %>%
  cluster_trajectory_data(K = 3)
#> 0 sec elapsed
#> [1] 1
#> [1] "e_step time"
#> 0.61 sec elapsed
#> [1] -Inf
#> [1] "m_step time"
#> 0.05 sec elapsed
#> [1] 2
#> [1] "e_step time"
#> 0.75 sec elapsed
#> [1] 3533.686
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 3
#> [1] "e_step time"
#> 0.76 sec elapsed
#> 2.2 sec elapsed


cluster_means <-
  extract_cluster_means(em_results)

nested_trajectory_data %>%
  bind_cols(as_tibble(em_results$Pik)) %>%
  mutate(curve_i = row_number()) %>%
  pivot_longer(names_to = "pred_cluster", values_to = "prob", matches("\\d")) %>%
  mutate(pred_cluster = parse_number(pred_cluster)) %>%
  group_by(curve_i) %>%
  filter(prob == max(prob)) %>%
  ungroup() %>%
  count(cluster, pred_cluster)
#> # A tibble: 3 x 3
#>   cluster pred_cluster     n
#>     <dbl>        <dbl> <int>
#> 1       1            2     7
#> 2       2            3     4
#> 3       3            1     9

nested_trajectory_data %>%
  bind_cols(as_tibble(em_results$Pik)) %>%
  mutate(curve_i = row_number()) %>%
  pivot_longer(names_to = "pred_cluster", values_to = "prob", matches("\\d")) %>%
  mutate(pred_cluster = parse_number(pred_cluster)) %>%
  group_by(curve_i) %>%
  filter(prob == max(prob)) %>%
  unnest(cols = c(x, y)) %>%
ggplot(aes(x = x, y = y, group = curve_i, colour = factor(cluster))) +
  geom_path() +
  facet_wrap(~ pred_cluster)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r


ggplot(cluster_means, aes(x = V1, y = V2, colour = factor(cluster))) +
  geom_path()+
  facet_wrap(~ cluster)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

``` r

new_nested_trajectory_data <-
  generate_sample_data(cluster_controls) %>%
  select(curve_i, x, y, cluster = cluster_num)


new_trajectory_data <- new_nested_trajectory_data %>% unnest(cols = c(x, y))

new_data_fit <- fit_new_data(new_trajectory_data, em_results)

new_data_fit %>%
  count(cluster, cluster_assigned)
#> # A tibble: 3 x 3
#>   cluster cluster_assigned     n
#>     <dbl>            <dbl> <int>
#> 1       1                2    43
#> 2       2                3    41
#> 3       3                1    36
```

``` r
# replace this with actual nfl route that has been transformed properly. Its incoming
fit_new_data(new_trajectory_data, nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))
#> # A tibble: 120 x 5
#>    curve_i cluster           data cluster_assigned route_name
#>      <int>   <dbl> <list<df[,2]>>            <dbl> <chr>     
#>  1       1       1       [40 x 2]               26 deep_out  
#>  2       2       2       [37 x 2]               24 quick_out 
#>  3       3       3       [41 x 2]                9 comeback  
#>  4       4       2       [42 x 2]               24 quick_out 
#>  5       5       1       [37 x 2]               26 deep_out  
#>  6       6       3       [43 x 2]                9 comeback  
#>  7       7       2       [49 x 2]               24 quick_out 
#>  8       8       3       [47 x 2]                9 comeback  
#>  9       9       3       [43 x 2]                9 comeback  
#> 10      10       1       [38 x 2]               26 deep_out  
#> # ... with 110 more rows
```
