library(tidyverse)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(rnaturalearthdata)

tracks_pts <- read_csv("data/SSM.csv") |>
  mutate(
    TOPPID = factor(TOPPID),
    Date = as.Date(Date, format = "%d-%b-%Y")
  ) |>
  arrange(TOPPID, Date) |>
  vect(geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

tracks <- unique(tracks_pts$TOPPID) |>
  map(\(id) {
    ln <- as.lines(tracks_pts[tracks_pts$TOPPID == id, ])
    values(ln) <- data.frame(TOPPID = id)
    ln
  }) |>
  reduce(rbind)

land <- ne_countries(scale = "medium", returnclass = "sv")

bbox <- ext(tracks) + 3

eez <- vect("data/eez/eez_v12_lowres.shp") |>
  crop(bbox)

writeVector(tracks, "data/tracks/tracks.shp", overwrite = TRUE)
writeVector(land, "data/land/land.shp", overwrite = TRUE)

ggplot() +
  geom_spatvector(data = land, fill = "gray80", color = "gray60", linewidth = 0.3) +
  geom_spatvector(data = eez, fill = NA, color = "steelblue", linewidth = 0.3) +
  geom_spatvector(
    data = tracks,
    aes(color = TOPPID),
    linewidth = 0.6, alpha = 0.8
  ) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax),
    expand = FALSE
  ) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Atlantic Bluefin Tuna Tracks",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()
