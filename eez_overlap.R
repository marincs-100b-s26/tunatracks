library(tidyverse)
library(terra)

tracks <- vect("data/tracks/tracks.shp")
eez <- vect("data/eez/eez.shp")

# Clip track segments to EEZ polygons; result has one row per track x EEZ combo
in_eez <- intersect(tracks, eez) |>
  as_tibble() |>
  mutate(length = perim(intersect(tracks, eez))) |>
  select(TOPPID, GEONAME, length)

# High Seas = total track length minus length inside any EEZ
total_length <- as_tibble(tracks) |>
  mutate(total = perim(tracks)) |>
  select(TOPPID, total)

high_seas <- in_eez |>
  group_by(TOPPID) |>
  summarize(eez_total = sum(length)) |>
  right_join(total_length, by = "TOPPID") |>
  mutate(
    eez_total = replace_na(eez_total, 0),
    length = total - eez_total,
    GEONAME = "High Seas"
  ) |>
  select(TOPPID, GEONAME, length)

# Fraction of track length per tuna per zone
zone_fractions <- bind_rows(in_eez, high_seas) |>
  group_by(TOPPID) |>
  mutate(fraction = length / sum(length)) |>
  ungroup()

# Number of distinct EEZs visited per tuna (excluding High Seas)
n_eez_visited <- zone_fractions |>
  filter(GEONAME != "High Seas") |>
  count(TOPPID, name = "n_eez")

# Order tunas by descending High Seas fraction; put High Seas last in stack
toppid_order <- zone_fractions |>
  filter(GEONAME == "High Seas") |>
  arrange(desc(fraction)) |>
  pull(TOPPID)

zone_fractions <- zone_fractions |>
  mutate(
    TOPPID = fct_relevel(TOPPID, toppid_order),
    GEONAME = fct_relevel(GEONAME, "High Seas", after = Inf)
  )

n_zones <- n_distinct(zone_fractions$GEONAME)
colors <- c(
  setNames(
    scales::hue_pal()(n_zones - 1),
    levels(zone_fractions$GEONAME)[levels(zone_fractions$GEONAME) != "High Seas"]
  ),
  "High Seas" = "gray80"
)

ggplot(zone_fractions, aes(x = TOPPID, y = fraction, fill = GEONAME)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Fraction of migration per EEZ",
    x = "Tuna ID", y = "Fraction of track length",
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total track length per EEZ summed across all tunas
eez_totals <- zone_fractions |>
  group_by(GEONAME) |>
  summarize(total_length = sum(length)) |>
  arrange(desc(total_length)) |>
  mutate(GEONAME = fct_inorder(GEONAME)) %>%
  filter(total_length >= 5e6)

ggplot(eez_totals, aes(x = GEONAME, y = total_length, fill = GEONAME)) +
  geom_col() +
  scale_fill_manual(values = colors, guide = "none") +
  labs(
    title = "Total track length by EEZ",
    x = NULL, y = "Total track length (degrees)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



