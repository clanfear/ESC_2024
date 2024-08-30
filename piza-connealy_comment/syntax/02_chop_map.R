library(maptiles)
library(tidyterra)
library(ggrepel)

mark_color <- "#333d4d"
label_bg_color <- "#fff5e5"
seattle_tracts <- tigris::tracts(state = "WA", county = "King", cb = TRUE) |>
  filter(as.numeric(GEOID) < "53033013000") 
ch_tracts <- seattle_tracts |>
  filter(GEOID %in% c("53033007301", "53033008700"))
ch_boundaries <- st_read("./piza-connealy_comment/data/raw/ch_boundaries/ch_boundaries.shp") %>%
  st_transform(4326)
east_precinct <- tibble(x = -122.3169972, y = 47.6150271) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) 
ch_tip <- tibble(x = -122.3168, y = 47.6188) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326)


left_plot <- ggplot() +
  geom_spatraster_rgb(data = get_tiles(seattle_tracts, provider = "Esri.WorldStreetMap", zoom = 13, crop = TRUE)) +
  geom_sf(data = ch_boundaries |> filter(name == "CHOP"), color = mark_color, fill = NA, linewidth = 0.5) +
  geom_label_repel(
    data = ch_tip,
    aes(geometry = geometry),
    label = "CHOP Zone",
    stat = "sf_coordinates",
    min.segment.length = 0,
    nudge_x = 0.02,
    nudge_y = 0.02,
    color = mark_color,
    fill = label_bg_color,
    family = "EB Garamond",
    size = 8
  ) +
  theme_void()
right_plot <- ggplot() +
  geom_spatraster_rgb(data = get_tiles(ch_tracts, provider ="Esri.WorldStreetMap", zoom = 15, crop = TRUE)) +
  geom_sf(data = ch_boundaries |> filter(name == "CHOP"), color = mark_color, fill = NA, linewidth = 0.5) +
  geom_sf(data = east_precinct, color = mark_color) +
  ggrepel::geom_label_repel(
    data = east_precinct,
    aes(geometry = geometry),
    label = "East Precinct\nBuilding",
    stat = "sf_coordinates",
    min.segment.length = 0,
    nudge_x = 0.005,
    nudge_y = -0.005,
    color = mark_color,
    fill = label_bg_color,
    lineheight = 0.25,
    family = "EB Garamond",
    size = 9
  ) +
  ggrepel::geom_label_repel(
    data = ch_tip,
    aes(geometry = geometry),
    label = "CHOP Zone",
    stat = "sf_coordinates",
    min.segment.length = 0,
    nudge_x = 0.006,
    nudge_y = 0.003,
    color = mark_color,
    fill = label_bg_color,
    family = "EB Garamond",
    size = 9
  ) +
  theme_void()
chop_map <- left_plot + right_plot & 
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"))

ggsave("./piza-connealy_comment/slides/img/chop_map.png", device = ragg::agg_png, width = 16, height = 9, units = "cm")
