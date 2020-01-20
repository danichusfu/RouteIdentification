
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
library(readr)

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
#> 1.31 sec elapsed
#> [1] -Inf
#> [1] "m_step time"
#> 0.08 sec elapsed
#> [1] 2
#> [1] "e_step time"
#> 0.95 sec elapsed
#> [1] -606.7129
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 3
#> [1] "e_step time"
#> 1.02 sec elapsed
#> [1] -601.8881
#> [1] "m_step time"
#> 0.06 sec elapsed
#> [1] 4
#> [1] "e_step time"
#> 0.82 sec elapsed
#> [1] -594.2849
#> [1] "m_step time"
#> 0.04 sec elapsed
#> [1] 5
#> [1] "e_step time"
#> 1.01 sec elapsed
#> [1] -592.1602
#> [1] "m_step time"
#> 0.05 sec elapsed
#> [1] 6
#> [1] "e_step time"
#> 1.23 sec elapsed
#> [1] -592.253
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 7
#> [1] "e_step time"
#> 1.13 sec elapsed
#> [1] -592.3209
#> [1] "m_step time"
#> 0.08 sec elapsed
#> [1] 8
#> [1] "e_step time"
#> 0.93 sec elapsed
#> [1] -592.3846
#> [1] "m_step time"
#> 0.05 sec elapsed
#> [1] 9
#> [1] "e_step time"
#> 1 sec elapsed
#> [1] -592.3089
#> [1] "m_step time"
#> 0.05 sec elapsed
#> [1] 10
#> [1] "e_step time"
#> 1.08 sec elapsed
#> [1] -592.1239
#> [1] "m_step time"
#> 0.04 sec elapsed
#> [1] 11
#> [1] "e_step time"
#> 1.56 sec elapsed
#> [1] -592.4983
#> [1] "m_step time"
#> 0.05 sec elapsed
#> [1] 12
#> [1] "e_step time"
#> 1.08 sec elapsed
#> [1] -592.5133
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 13
#> [1] "e_step time"
#> 0.78 sec elapsed
#> 14.53 sec elapsed


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
#> # A tibble: 4 x 3
#>   cluster pred_cluster     n
#>     <dbl>        <dbl> <int>
#> 1       1            1     5
#> 2       2            2     4
#> 3       2            3     2
#> 4       3            1     9

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
#> # A tibble: 4 x 3
#>   cluster cluster_assigned     n
#>     <dbl>            <dbl> <int>
#> 1       1                1    33
#> 2       2                2    42
#> 3       2                3     4
#> 4       3                1    41
```

``` r
# replace this with actual nfl route that has been transformed properly. Its incoming
fit_new_data(new_trajectory_data, nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))
#> # A tibble: 120 x 5
#>    curve_i cluster           data cluster_assigned route_name
#>      <int>   <dbl> <list<df[,2]>>            <dbl> <chr>     
#>  1       1       1       [37 x 2]               26 deep_out  
#>  2       2       3       [44 x 2]               26 deep_out  
#>  3       3       1       [48 x 2]               26 deep_out  
#>  4       4       3       [36 x 2]               26 deep_out  
#>  5       5       2       [45 x 2]               26 deep_out  
#>  6       6       2       [39 x 2]               26 deep_out  
#>  7       7       3       [47 x 2]               26 deep_out  
#>  8       8       1       [47 x 2]               26 deep_out  
#>  9       9       2       [49 x 2]               26 deep_out  
#> 10      10       2       [46 x 2]               26 deep_out  
#> # ... with 110 more rows
```

``` r
# Use online sample data from big data bowl

# list all the files
#tracking_files <- list.files(path = "Data/", pattern = "tracking_.*\\.csv")

# Load all the data
routes_data <- 
  # create the tibble with the file names
  tibble(file_name = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv") %>%
  # read the data in nested
  mutate(data = map(file_name, read_routes_from_csv)) %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = c(data))

# transform our curves
routes_data <-
  routes_data %>%
  mutate(row = row_number()) %>%
  mutate(data = pmap(list(data, team, direction_left, line_of_scrimmage), 
                     ~ cut_plays(..1) %>%
                       flip_field(., ..2, ..3, ..4)),
         n = map_dbl(data, nrow)) %>%
  filter(n >= 2) %>%
  # left side of field is TRUE
  mutate(data_same_sideline = purrr::map(data, 
                                         ~ mutate(., 
                                                  sof = 160/6 > first(y),
                                                  y = if_else(sof, 160/3 - y, y),
                                                  y = y - first(y)
                                         ) %>%
                                           dplyr::select(-sof)))  %>%
  arrange(row)


routes_data <-
  routes_data %>%
  ungroup() %>%
  select(-row)


routes_data %>%
  mutate(curve_num = row_number()) %>%
  unnest(cols = c(data_same_sideline)) %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))
#> # A tibble: 394 x 4
#>    curve_num           data cluster_assigned route_name
#>        <int> <list<df[,2]>>            <dbl> <chr>     
#>  1         1       [63 x 2]               30 dig/over  
#>  2         2       [63 x 2]                5 go/seam   
#>  3         3       [63 x 2]               27 corner    
#>  4         4       [63 x 2]               11 flat      
#>  5         5       [63 x 2]                5 go/seam   
#>  6         6       [45 x 2]               27 corner    
#>  7         7       [45 x 2]                4 flat      
#>  8         8       [45 x 2]                2 dig/over  
#>  9         9       [45 x 2]               18 flat      
#> 10        10       [45 x 2]               29 deep_out  
#> # ... with 384 more rows
```
