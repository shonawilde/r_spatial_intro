pacman::p_load(
  tidyverse, 
  sf, 
  gissr,
  mobilemeasr, 
  shonarrr,
  ggnewscale
)

options("rgdal_show_exportToProj4_warnings"="none") 

setwd("~/Google Drive/My Drive/data/")


# LOAD ----

# road network
st_highways <- st_read("osm/osm_filtered/london_highways_tidy.geojson")

# river
st_thames <- st_read("spatial/uk/river_thames_cropped.geojson")

st_thames %>% 
  leaflet_plot()


# ulez
st_ulez <- st_read(
  "https://data.london.gov.uk/download/ultra_low_emissions_zone_expansion_new/91a28302-0606-4d61-b4c0-bbe5d93a0e4e/Ultra_Low_Emissions_Zone_Expansion.json"
) %>% 
  mutate(
    zone = "ULEZ"
  )

st_ulez %>% 
  leaflet_plot()

# congestion charge zone
st_cz <- st_read("spatial/uk/st_congestion_zone.json")

st_cz %>% 
  leaflet_plot()

# mobile data
st_loops <- read_rds("st_mobile.rds")

st_loops %>% 
  glimpse()

# BBOX OF LOOPS ----
st_loops_lines <- st_loops %>% 
  filter(loop_id == 1) %>%
  st_points_to_lines(
    group_by = c("location", "loop_id")
  ) %>% 
  ungroup() 

# plot
st_loops_lines %>% 
  leaflet_plot_coloured(
    "location",
    palette = ggplot2_colours(),
    opacity = 1
  )

st_bbox(st_loops_lines)

st_loops_lines %>% 
  st_get_boundary() %>% 
  leaflet_plot()

# get bounding boxes of each location
st_bbox <- st_loops_lines %>% 
  nest_by(location) %>% 
  mutate(
    data = list(
      st_get_boundary(data, buffer = 500)
    )
  ) %>% 
  unnest(data) %>%
  mutate(
    location = case_when(
      location == "central_london" ~ "Central",
      location == "ilford" ~ "Outer",
      T ~ location
    ),
  ) %>% 
  st_as_sf() 

# check
st_bbox %>% 
  leaflet_plot_coloured(
    "location",
    palette = ggplot2_colours(),
    opacity = 1
  )

# PLOT ----

# ocean palette
pal <- c("#014961", "#009CA8", "#C2DACC", "#DC7467", "#EB0132", "gray50")

# continuous
pal_contin <- colorRampPalette(pal)(56)


# build plot
st_highways %>% 
  mutate(
    highway = str_to_sentence(highway),
    highway = factor(highway,
                     levels = c("Motorway",
                                "Trunk",
                                "Primary",
                                "Secondary",
                                "Tertiary",
                                "Residential") %>% rev()
    )
  ) %>% 
  ggplot() +
  geom_sf(
    aes(linewidth = highway, col = zone),
  #  col = "gray50",
    show.legend = F
  ) +
  geom_sf(
    data = st_thames,
    fill = "lightskyblue",
    col = "lightskyblue"
  ) +
  geom_sf(
    data = st_ulez,
    alpha = 0.1,
    col = "darkgoldenrod1",
    linewidth = 1
  ) + 
  geom_sf(
    data = st_cz,
    alpha = 0.1,
    col = "darkmagenta",
    linewidth = 1
  ) + 
  new_scale_color() +
  geom_sf(
    data = st_bbox,
    aes(
      col = location,
      fill = location
    ),
    alpha = 0.5,
    linewidth = 1
  ) +
  scale_linewidth_discrete(
    range = c(0.1, 0.7),
    name = "Highway"
  ) +
  scale_colour_manual(
    values = pal[c(5, 2)],
    name = "Location"
  ) +
  scale_fill_manual(
    values = pal[c(5, 2)],
    name = "Location"
  ) +
  labs(
    caption = "© OpenStreetMap contributors. Distributed under the Open Data Commons Open Database License (ODbL) v1.0"
  ) +
  theme_shona_void() +
  theme(
    legend.title = element_text(size = 12, vjust = 0.5),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10)
  ) +
  guides(
    linewidth = "none"
  )


# save
# ggsave(
#   "../PostDoc/intro_to_spatial_data/intro_to_spatial_data_sw/plots/london_map.png",
#   width = 10,
#   height = 8
# )



# COLOUR ROADS BY ULEZ ---

# join roads to ULEZ
st_highways_join <- st_highways %>% 
  st_join(
    st_ulez,
    st_within
  )

# check
st_highways_join %>% 
  sample_n(1000) %>% 
  leaflet_plot_coloured("zone")

# replot
st_highways_join %>% 
  mutate(
    highway = str_to_sentence(highway),
    highway = factor(highway,
                     levels = c("Motorway",
                                "Trunk",
                                "Primary",
                                "Secondary",
                                "Tertiary",
                                "Residential") %>% rev()
    )
  ) %>% 
  ggplot() +
  geom_sf(
    aes(linewidth = highway, col = zone),
    #col = "gray50",
    show.legend = F
  ) +
  scale_colour_manual(
    values = c("forestgreen", "gray50")
  ) +
  geom_sf(
    data = st_thames,
    fill = "lightskyblue",
    col = "lightskyblue"
  ) +
  geom_sf(
    data = st_ulez,
    alpha = 0.1,
    col = "darkgoldenrod1",
    linewidth = 1
  ) + 
  geom_sf(
    data = st_cz,
    alpha = 0.1,
    col = "darkmagenta",
    linewidth = 1
  ) + 
  new_scale_color() +
  geom_sf(
    data = st_bbox,
    aes(
      col = location,
      fill = location
    ),
    alpha = 0.5,
    linewidth = 1
  ) +
  scale_linewidth_discrete(
    range = c(0.1, 0.7),
    name = "Highway"
  ) +
  scale_colour_manual(
    values = pal[c(5, 2)],
    name = "Location"
  ) +
  scale_fill_manual(
    values = pal[c(5, 2)],
    name = "Location"
  ) +
  labs(
    caption = "© OpenStreetMap contributors. Distributed under the Open Data Commons Open Database License (ODbL) v1.0"
  ) +
  theme_shona_void() +
  theme(
    legend.title = element_text(size = 12, vjust = 0.5),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10)
  ) +
  guides(
    linewidth = "none"
  )
