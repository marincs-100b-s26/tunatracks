library(tidyverse)
library(terra)
library(sf)

eez <- vect("data/eez/eez_v12_lowres.shp")

# Strip parenthetical territory suffix, e.g.
# "Portuguese Exclusive Economic Zone (Madeira)" -> "Portuguese Exclusive Economic Zone"
eez$GEONAME_BASE <- str_remove(eez$GEONAME, "\\s*\\(.*\\)$")

# Planar geometry avoids spherical edge-crossing errors during union
sf_use_s2(FALSE)

# Dissolve polygons that share the same base name
eez_merged <- eez[, "GEONAME_BASE"] |>
  st_as_sf() |>
  st_make_valid() |>
  group_by(GEONAME_BASE) |>
  summarize(do_union = TRUE) |>
  st_buffer(dist = 0) |>
  rename(GEONAME = GEONAME_BASE) |>
  vect()

writeVector(eez_merged, "data/eez/eez.shp", overwrite = TRUE)
