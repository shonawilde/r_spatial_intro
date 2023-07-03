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

# setwd("PostDoc/vehicle_emissions/london_campaign_analysis/data/")


# LOAD ----

# van data
st_mobile <- read_rds("st_mobile.rds")

# glimpse
st_mobile %>% 
  glimpse()

# plot
st_mobile %>% 
  leaflet_plot_coloured("nox_ppb")


# REMOVE BACKGROUND ----

# nest by loop
st_background <- st_mobile %>% 
  group_nest(
    location, loop_id
  ) %>% 
  rowwise() %>% 
  mutate(
    data = list(calculate_rolling_background(
      df = data,
      cols = c(nox_ppb, co2, co2_ppb), 
      width = 300)
    )
  )


# unnest
st_background_unnest <- st_background %>% 
  unnest(data) %>% 
  st_as_sf()

st_background_unnest %>% 
  glimpse()

# make long
st_background_long <- st_background_unnest %>% 
  pivot_longer(
    c(contains("background"), nox_ppb, co2, co2_ppb),
    names_to = c("variable")
  ) %>% 
  mutate(
    type = if_else(str_detect(variable, "background"), "background", "value"),
    variable = str_remove(variable, "background_")
  ) %>% 
  pivot_wider(
    names_from = "type",
    values_from = c("value")
  ) %>%
  glimpse()


# subtract background
st_background_subtract <- st_background_long %>% 
  mutate(
    value_enhance = value-background
  )

# make wide
st_background_subtract_wide <- st_background_subtract %>% 
  select(
    -c(value, background)
  ) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "value_enhance"
  ) %>% 
  st_as_sf() %>% 
  glimpse()


# check
st_background_subtract_wide %>% 
  filter(
    loop_id == 1
  ) %>% 
  leaflet_plot_coloured("co2_ppb")


# PLOT ----

# ocean palette
pal <- c("#014961", "#009CA8", "#C2DACC", "#DC7467", "#EB0132", "gray50")

# continuous
pal_contin <- colorRampPalette(pal)(56)


# var labels
delta_labs <- c(
  ch4 = "Delta*CH[4]~(ppb)",
  co = "Delta*CO~(ppb)",
  co2 = "Delta*CO[2]~(ppm)",
  no_ppb = "Delta*NO~(ppb)",
  no2_ppb = "Delta*NO[2]~(ppb)",
  nox_ppb = "Delta*NO[x]~(ppb)",
  speed_kmh = "Speed~(km~h^-1)"
)


# co2 time series
st_background_long %>% 
  filter(
    variable == "co2"
  ) %>% 
  ggplot() +
  geom_line(
    aes(date, value),
    col = "gray50"
  ) +
  geom_line(
    aes(date, background, col = "Background"),
  ) +
  facet_wrap(
    ~loop_id,
    scales = "free_x"
  ) +
  scale_colour_manual(
    values = "firebrick1"
  ) +
  labs(
    x = "Date",
    y = expression(CO[2]~(ppm)),
  ) +
  theme_shona_less_minimal("top") +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 12)
  )

# nox time series
st_background_long %>% 
  filter(
    variable == "nox_ppb"
  ) %>% 
  ggplot() +
  geom_line(
    aes(date, value),
    col = "gray50"
  ) +
  geom_line(
    aes(date, background, col = "Background"),
  ) +
  facet_wrap(
    ~loop_id,
    scales = "free_x"
  ) +
  scale_colour_manual(
    values = "darkcyan"
  ) +
  labs(
    x = "Date",
    y = expression(NO[x]~(ppb)),
  ) +
  theme_shona_less_minimal("top") +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 12)
  )


# plot enhancement fora singel loop
st_background_subtract_wide %>% 
  st_drop_geometry() %>% 
  filter(loop_id == 1) %>% 
  pivot_longer(
    cols = c(nox_ppb, co2),
    names_to = "variable"
  ) %>% 
  ggplot() +
  geom_line(
    aes(date, value, col = variable),
    show.legend = F
  ) +
  facet_wrap(
    ~variable,
    scales = "free_y",
    ncol = 1,
    labeller = as_labeller(delta_labs, label_parsed)
  ) +
  scale_color_manual(
    values = pal[c(5, 2)]
  ) +
  labs(
    x = "Date",
    y = "Enhancement"
  ) +
  theme_shona_less_minimal()


# SAVE ----
write_rds(
  st_background_subtract_wide,
  "../../mobile_meas_framework/example_workflow/st_background_subtract_wide.rds"
  
)



