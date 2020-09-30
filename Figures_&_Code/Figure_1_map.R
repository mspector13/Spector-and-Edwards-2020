library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-122.5, -115.3), ylim = c(31.5, 37.3), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-124.5, -115.3), ylim = c(31, 37.3), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, 
                   pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.01, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = -122, y = 33, label = "Pacific Ocean", 
           fontface = "italic", color = "grey22", size = 6) +
  annotate(geom = "text", x = -118, y = 35.5, label = "USA", 
           fontface = "bold", color = "grey22", size = 6) +
  annotate(geom = "text", x = -116, y = 32.25, label = "MEX", 
           fontface = "bold", color = "grey22", size = 6) +
  annotate(geom = "text", x = -120.13, y = 34.65, label = "Point Conception", 
           color = "grey22", size = 2) +
  annotate(geom = "text", x = -122.6, y = 36.5, label = "Stillwater Cove", 
          fontface = "bold.italic", color = "grey22", size = 3) +
  annotate(geom = "text", x = -117.78, y = 32.74, label = "Point Loma", 
           fontface = "bold.italic", color = "grey22", size = 3) +
  annotate(geom = "text", x = -117.45, y = 31.71, label = "Campo Kennedy", 
           fontface = "bold.italic", color = "grey22", size = 3) +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_rect(fill = NA))

ggsave("map.pdf")
