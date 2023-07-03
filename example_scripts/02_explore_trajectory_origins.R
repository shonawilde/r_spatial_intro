pacman::p_load(
  tidyverse,
  sf,
  shonarrr,
  mobilemeasr,
  gissr,
  janitor,
  wesanderson,
  mapview
)

sethd()

# setwd("PostDoc/PM2.5_investigations/data/")


# LOAD ----
st_traj <- read_rds("data/st_traj_join.rds") %>% 
  mutate(
    name = replace_na(name, "Ocean")
  )

# map
st_map <- map_as_st()

st_map %>% 
  leaflet_plot()


# EXPLORE ----

# add group identifiers
st_traj_group <- st_traj %>% 
  group_by(date) %>% 
  add_group_identifier() %>%
  mutate(
    main_europe = case_when(
      continent == "Europe" & !name %in% c("Greenland", "Iceland", "United Kingdom", "Ireland", "Isle of Man", "Faeroe Is.") ~ T,
      TRUE ~ F
    )
  ) %>% 
  group_by(group) %>% 
  mutate(
    any_europe = any(main_europe)
  ) %>% 
  st_as_sf() %>% 
  ungroup()


st_traj_group %>% 
  glimpse()

# check
st_traj_group %>% 
  filter(
    main_europe
  ) %>% 
  sample_n(100) %>% 
  leaflet_plot_coloured("name")


# proportion of hours that touch mainland Europe
st_traj_group %>% 
  st_drop_geometry() %>% 
  calculate_group_split(main_europe)

# proportion of trajectories that touch mainland europe
st_traj_group %>% 
  st_distinct(group, any_europe) %>% 
  calculate_group_split(any_europe)

# check
st_traj_group %>% 
  filter(group %in% c(149:177)) %>% 
  leaflet_plot_coloured("main_europe")

# where does most air come from
df_country_percent <- st_traj_group %>% 
  st_drop_geometry() %>% 
  calculate_group_split(name)


# percent of trajectories a country is passed over
n_traj <- st_traj_group %>% 
  st_distinct(group) %>% 
  nrow()
  
df_country_count <- st_traj_group %>% 
  st_drop_geometry() %>% 
  group_by(group) %>% 
  distinct(name) %>% 
  ungroup() %>% 
  count(name) %>% 
  mutate(
    percent = (n/n_traj)*100
  ) 

# PLOT ----

df_country_percent %>% 
  filter(percent > 0.5) %>% 
  ungroup() %>% 
  mutate(
    name = fct_reorder(name, percent, .desc = T)
  ) %>% 
  ggplot() +
  geom_col(
    aes(name, percent)
  ) 


df_country_count %>% 
  filter(n > 1) %>% 
  arrange(desc(n)) %>% 
  mutate(
    name = fct_reorder(name, n)
  ) %>% 
  print() %>% 
  ggplot() +
  geom_col(
    aes(percent, name)
  ) +
  labs(
    title = "Percentage of trajectories that pass over each country "
  )

# on map
pal <- wes_palette("Zissou1", 100, type = "continuous")

st_map %>% 
  left_join(
    df_country_count %>% 
      filter(
        !name == "United Kingdom"
      ),
    by = "name"
  ) %>% 
  st_as_sf() %>%  
  ggplot() +
  geom_sf(
    aes(fill = percent,
        col = percent),
    alpha = 0.65,
  #  col = "w7hite",
    size = 0.3
  ) +
  scale_fill_gradientn(
    colors = pal,
    name = "% trajectories"
  ) +
  scale_colour_gradientn(
    colors = pal,
    name = "% trajectories"
  ) +
  coord_sf(crs= "+proj=ortho +lat_0=30 +lon_0=0") +
  labs(
    title = "Percentage of trajectories that pass over each country "
  ) +
  theme_shona_white_big("top") +
  theme(
    legend.title = element_text(vjust = 1, size = 12.5),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_line(colour = "gray"),
    legend.text = element_text(size = 12.5)
  ) +
  guides(
    fill = guide_colorbar(barwidth = 15)
  ) 

# save 
# ggsave("../plots/percent_traj_map.png",
#        width = 8,
#        height = 8)


# plot single trajectory
map <- st_traj_group %>% 
  filter(group %in% c(2916)) %>% 
  leaflet_plot_coloured(
    "name", 
    pal = wes_palette("Darjeeling1") %>% as.character() %>% rev()
  )


# save
# mapshot(
#   map,
#   file = "../plots/traj_plot.png",
#   remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
#                       "drawToolbar", "easyButton")
# )
