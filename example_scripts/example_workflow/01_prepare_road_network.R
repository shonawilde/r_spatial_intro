pacman::p_load(
  tidyverse,
  sf,
  shonarrr,
  mobilemeasr,
  gissr,
  janitor,
  plotly,
  openair,
  leaflet,
  ggridges
)

sethd()

setwd("PostDoc/vehicle_emissions/london_campaign_analysis/data/")


# LOAD ----

#  road network
st_roads <- read_rds("../../london_measurements/data/road_network/st_roads_route1_central_london.rds")


# filter
st_roads_filt <- st_roads %>% 
  filter(
    name == "Victoria Embankment"
  ) 

# check
st_roads_filt %>% 
  leaflet_plot_coloured("id")

st_roads_filt %>% 
  st_union() %>% 
  st_as_sf() %>% 
  leaflet_plot()


# CREATE EQUALLY SPACED POINTS ----
st_roads_points <- st_roads_filt %>% 
  nest_by(
    name
  ) %>%  
   mutate(
    data = list(
      st_union(data, by_feature = F) %>%
        st_as_sf()
    ),
    data = list(
      st_interpolate(
        data,
        distance = 50
      )
    )
  ) %>% 
  unnest(data) %>% 
  st_as_sf()


# check
st_roads_points %>% 
  leaflet_plot()


# SAVE ----
write_rds(
  st_roads_points,
  "../../mobile_meas_framework/example_workflow/st_roads_points.rds"
)

