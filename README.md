
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RouteIdentification

<!-- badges: start -->

<!-- badges: end -->

The goal of RouteIdentification is to clustery trajectory data sets like
those found in sports analytics tracking data. This work was developed
as part of the first Big Data Bowl and the methodology is written about
in
[JQAS](https://www.degruyter.com/view/j/jqas.ahead-of-print/jqas-2019-0047/jqas-2019-0047.xml?format=INT)

## Installation

<!--You can install the released version of RouteIdentification from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RouteIdentification")
```
 -->

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
#> 0.53 sec elapsed
#> [1] -Inf
#> [1] "m_step time"
#> 0.03 sec elapsed
#> [1] 2
#> [1] "e_step time"
#> 0.53 sec elapsed
#> [1] 3647.155
#> [1] "m_step time"
#> 0.02 sec elapsed
#> [1] 3
#> [1] "e_step time"
#> 0.52 sec elapsed
#> 1.63 sec elapsed


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
#> 1       1            1    12
#> 2       2            3     4
#> 3       3            2     4

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
#> 1       1                1    46
#> 2       2                3    31
#> 3       3                2    43
```

``` r
# replace this with actual nfl route that has been transformed properly. Its incoming
fit_new_data(new_trajectory_data, nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))
#> # A tibble: 120 x 5
#>    curve_i cluster           data cluster_assigned route_name
#>      <int>   <dbl> <list<df[,2]>>            <dbl> <chr>     
#>  1       1       1       [43 x 2]               18 flat      
#>  2       2       3       [41 x 2]               26 deep_out  
#>  3       3       1       [37 x 2]               18 flat      
#>  4       4       1       [39 x 2]               18 flat      
#>  5       5       1       [47 x 2]               18 flat      
#>  6       6       2       [49 x 2]               26 deep_out  
#>  7       7       3       [48 x 2]               26 deep_out  
#>  8       8       1       [46 x 2]               18 flat      
#>  9       9       1       [49 x 2]               18 flat      
#> 10      10       2       [43 x 2]               26 deep_out  
#> # ... with 110 more rows
```

## Now with NFL sample data

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

## Now with the higlight data from 903124

``` r
# Use online sample data from big data bowl

# list all the files
#tracking_files <- list.files(path = "Data/", pattern = "tracking_.*\\.csv")

# Load all the data
routes_data <- 
  # create the tibble with the file names
  tibble(file_name = "https://raw.githubusercontent.com/danichusfu/NFL_Highlight_Tracking/master/Highligh_19_post.csv") %>%
  # read the data in nested
  mutate(data = map(file_name, read_routes_from_903124)) %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = c(data))
#> Warning: Missing column names filled in: 'X1' [1]

# transform our curves
routes_data <-
  routes_data %>%
  mutate(row = row_number()) %>%
  mutate(data = pmap(list(data, left, line_of_scrimmage), 
                     ~ cut_plays(..1) %>%
                       flip_field_903124(., ..2, ..3)),
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


fitted_clusters <-
  routes_data %>%
  mutate(curve_num = row_number()) %>%
  unnest(cols = c(data_same_sideline)) %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))

fitted_clusters
#> # A tibble: 76 x 4
#>    curve_num           data cluster_assigned route_name 
#>        <int> <list<df[,2]>>            <dbl> <chr>      
#>  1         1       [57 x 2]               25 corner     
#>  2         2       [57 x 2]               12 dig/over   
#>  3         3       [57 x 2]               28 blocking   
#>  4         4       [57 x 2]               19 blocking   
#>  5         5       [57 x 2]                4 flat       
#>  6         6       [61 x 2]                3 comeback   
#>  7         7       [61 x 2]               27 corner     
#>  8         8       [61 x 2]                8 go/vertical
#>  9         9       [61 x 2]               13 blocking   
#> 10        10       [61 x 2]                3 comeback   
#> # ... with 66 more rows

routes_data %>%
  select(displayName, gameId, playId) %>%
  bind_cols(fitted_clusters %>% select(route_name))
#> # A tibble: 76 x 4
#>    displayName         gameId playId route_name 
#>    <chr>                <dbl>  <dbl> <chr>      
#>  1 DeAndre Hopkins 2020010400   3187 corner     
#>  2 Kenny Stills    2020010400   3187 dig/over   
#>  3 Darren Fells    2020010400   3187 blocking   
#>  4 Carlos Hyde     2020010400   3187 blocking   
#>  5 DeAndre Carter  2020010400   3187 flat       
#>  6 Taiwan Jones    2020010400   4780 comeback   
#>  7 DeAndre Hopkins 2020010400   4780 corner     
#>  8 Kenny Stills    2020010400   4780 go/vertical
#>  9 Darren Fells    2020010400   4780 blocking   
#> 10 DeAndre Carter  2020010400   4780 comeback   
#> # ... with 66 more rows
```