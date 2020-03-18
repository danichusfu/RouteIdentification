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

## Another NFL example: NextGenStats Scraped Data (compliments to @903124S)

nfl_ngs_sample <- format_nfl_data(file_name = "https://raw.githubusercontent.com/danichusfu/NFL_Highlight_Tracking/master/Highlight_19_post.csv",
                                  data_source = "903124")

fitted_clusters <- nfl_ngs_sample %>%
  select(curve_num, x, y) %>%
  fit_new_data(nfl_em_results) %>%
  left_join(cluster_route_map, by = c("cluster_assigned" = "cluster"))

# Overview of the assigned routes
nfl_ngs_sample %>%
  nest(cols = -c(gameId, playId, displayName)) %>%
  select(gameId, playId, displayName) %>%
  bind_cols(fitted_clusters %>% select(route_name))
