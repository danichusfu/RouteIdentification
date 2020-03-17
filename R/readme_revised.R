# The revised readme code, based on modularized code

library(RouteIdentification)
library(readr)

# Generate data
nested_trajectory_data <- rand_centred_curves(n_clust = 3, n_curves = 20)

# Apply EM algorithm, either to generated data or appropriately formatted data
em_results <- driver_em_nested(nested_trajectory_data, K = 3)

# Grab the cluster means
cluster_means <- extract_cluster_means(em_results)

# Identify cluster assignments
cluster_assignments <- identify_clusters(nested_trajectory_data, em_results)

# Count cluster assignments
cluster_assignments %>%
  count(cluster, pred_cluster)

# Plot clusters assigments by assigned cluster mean
cluster_assignments %>%
  plot_curve_assign()

# Simple plot of just the cluster means, no other curves
cluster_means %>%
  ggplot(aes(x = V1, y = V2, colour = factor(cluster))) +
  geom_path() +
  facet_wrap(~ cluster)

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


## NFL Data example

# Load all the data
routes_data <- 
  # create the tibble with the file names
  tibble(file_name = "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv") %>%
  # read the data in nested
  mutate(data = map(file_name, read_routes_from_csv)) %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = c(data))

# Transform our curves
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

# Formatting fix
routes_data <-
  routes_data %>%
  ungroup() %>%
  select(-row)

# nfl_em_results and cluster_route_map are objects that exist within this package and can be called freely
routes_data %>%
  mutate(curve_num = row_number()) %>%
  unnest(cols = c(data_same_sideline)) %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))


## Another NFL example: NextGenStats Scraped Data (compliments to @903124S)

# Load all the data
routes_data <- 
  # create the tibble with the file names
  tibble(file_name = "https://raw.githubusercontent.com/danichusfu/NFL_Highlight_Tracking/master/Highlight_19_post.csv") %>%
  # read the data in nested
  mutate(data = map(file_name, read_routes_from_903124)) %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = c(data))

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

routes_data %>%
  select(displayName, gameId, playId) %>%
  bind_cols(fitted_clusters %>% select(route_name))