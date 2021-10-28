library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(patchwork)

# load germany's map
germany = ne_countries(scale = "medium",
                       type = "countries",
                       country = "Germany",
                       returnclass = "sf")
# load NPHH
path = "C:\\Users\\hi63tov\\Desktop\\RS_data\\shp\\NPHH_boundary.shp"
shp = st_read(path)
shp = st_transform(shp, crs = st_crs(germany))

# map of the NP within Germany
german_map = ggplot() +
        geom_sf(data = germany, aes(fill = "Germany")) +
        geom_sf(data = shp, aes(fill = "Study Area")) +
        labs(title = "Study area NPHH within Germany",
             fill = NULL) +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "tl", which_north = "true", 
                               style = north_arrow_fancy_orienteering) +
        theme_bw() +
        theme(legend.position = c(.9, .06))

# map of the study area
np = ggplot() +
        geom_sf(data = shp, fill = "turquoise") +
        labs(title = "Study area NPHH") +
        annotation_scale(location = "br") +
        annotation_north_arrow(location = "tl", which_north = "true", 
                               style = north_arrow_fancy_orienteering) +
        theme_bw()
