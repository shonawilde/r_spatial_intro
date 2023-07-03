pacman::p_load(
  tidyverse,
  sf,
  shonarrr,
  mobilemeasr,
  gissr,
  janitor,
  leaflet
)

sethd()

# setwd("PostDoc/vehicle_emissions/mobile_meas_framework/example_workflow/")

# LOAD -----

# background data
st_background_subtract_wide <- read_rds(
  "data/st_background_subtract_wide.rds"
  )

st_background_subtract_wide %>% 
  glimpse()

# road network
st_roads_points <- read_rds(
  "data/st_roads_points.rds"
)

#
st_roads_points %>% 
  leaflet_plot()

# road network
st_roads <- read_rds(
  "data/st_roads_route1_central_london.rds"
  )



# REGRESSION ----

# define formula for model
formula <- nox_ppb~co2_ppb


# DO 

# ordinary linear regression
df_results <- st_weighted_linear_regression(
  st = st_background_subtract_wide,
  location = st_roads_points,
  formula = formula,
  sigma = 100
)

# quantile regression
# df_results <- st_weighted_quantile_regression(
#   st = st_background_subtract_wide,
#   location = st_roads_points,
#   formula = formula,
#   tau = c(0.5, 0.75, 0.9, 0.95, 0.99),
#   sigma = 100
# )

# view
df_results

# as spatial
st_results <- df_results %>% 
  filter(
    !term == "intercept"
  ) %>% 
  st_from_df() %>% 
  st_join(
    st_roads,
    st_nearest_feature
  )

# PLOT -----
st_results %>% 
  # filter(
  #   quantile == 0.99
  # ) %>%
  leaflet_plot_coloured("value")


# PLOT ---

# mean across quantiles
df_quantile_mean <- st_results %>% 
  st_drop_geometry() %>% 
  group_by(quantile_name, quantile) %>% 
  group_modify(
    ~calculate_errors(.$value)
  ) 

# plot
df_quantile_mean %>% 
  ggplot() +
  geom_pointrange(
    aes(mean, as.factor(quantile), xmin = lower, xmax = upper),
    col = "firebrick1",
    size = 0.75
  ) +
  labs(
    y = expression(tau),
    x = expression(Delta*NO[x]/Delta*CO[2]~(by~vol.))
  ) +
  theme_shona_white_smart()
