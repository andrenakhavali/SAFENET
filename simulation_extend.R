# SAFENET EUROPE - MODELING EXTENT MAP
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

# Helper function for polygon creation
create_polygon <- function(lon_min, lon_max, lat_min, lat_max) {
  st_sfc(
    st_polygon(list(matrix(c(
      lon_min, lat_min,
      lon_max, lat_min,
      lon_max, lat_max,
      lon_min, lat_max,
      lon_min, lat_min
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
}

# Get and process base data
countries <- ne_countries(scale = 50, returnclass = "sf") %>%
  select(name) %>%
  st_buffer(0) %>%
  st_make_valid()

# Define and crop to Europe
europe_bbox <- st_bbox(c(xmin = -15, xmax = 40, ymin = 35, ymax = 70), crs = 4326)
europe_cropped <- countries %>% st_crop(europe_bbox) %>% st_make_valid()

# Island processing
cyprus <- europe_cropped %>% filter(name == "Cyprus")
europe_no_cy <- europe_cropped %>% filter(name != "Cyprus")

remove_islands <- list(
  create_polygon(1.0, 4.5, 38.0, 40.5),    # Balearic
  create_polygon(-18.0, -12.5, 27.0, 29.5), # Canary
  create_polygon(-8.0, -6.0, 61.0, 63.0),   # Faroe
  create_polygon(-2.0, 0.0, 59.0, 61.0),    # Shetland
  create_polygon(19.0, 28.0, 34.0, 37.0)    # Aegean islands
) %>% do.call(c, .) %>% st_union()

# Create final extent map
modeling_extent <- europe_no_cy %>%
  st_difference(remove_islands) %>%
  filter(!name %in% c("Turkey", "Iceland")) %>%
  bind_rows(cyprus) %>%
  st_make_valid()

# Minimalist plot focused on extent
ggplot() +
  geom_sf(data = modeling_extent, 
          fill = "#e5e5e5",        # Light gray fill
          color = "#404040",       # Dark borders
          linewidth = 0.2) +       # Thin boundary lines
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "SAFENET Europe Map - Modeling Extent")

# Save output
ggsave("Europe_Final/Modeling_Extent.png", 
       width = 8, height = 6, dpi = 300, bg = "white")