pacman::p_load(
  tidyverse,
  sf,
  shonarrr,
  mobilemeasr,
  gissr,
  janitor
)

sethd()

setwd("PostDoc/PM2.5_investigations/data/")


# LOAD ----

# trajectories
df_traj <- read_rds("trajectories.rds") %>%
  tibble() %>% 
  clean_names()

# as spatial
st_traj <- df_traj %>% 
  st_from_df(
    latitude = "lat",
    longitude = "lon",
    crs = 4326
  )

# map
st_map <- map_as_st() %>% 
  st_transform(4326)

plot(st_map)


# JOIN ----
sf_use_s2(F)

# point in polygon test
st_traj_join <- st_traj %>% 
 # sample_n(1000) %>% 
  st_join(
    st_map,
    st_intersects
  )

glimpse(st_traj_join)

# PLOT ----

# a single trajectory
st_traj %>% 
  filter(
    date == ymd_hms("2010-02-01 12:00:00")
  ) %>% 
  leaflet_plot_coloured("hour_inc")


# a single trajectory
st_traj_join %>% 
  filter(
    date == ymd_hms("2010-02-01 12:00:00")
  ) %>% 
  leaflet_plot_coloured("name", palette = c("red", "lightseagreen"))

# check
st_traj_join %>%
  sample_n(100) %>% 
  leaflet_plot_coloured("name")


# check no matches
st_traj_join %>% 
  filter(
    is.na(name)
  ) %>% 
  sample_n(1000) %>% 
  leaflet_plot()


# SAVE ----
# write_rds(st_traj_join, "intermediate_files/st_traj_join.rds")




