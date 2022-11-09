library(tidyverse)
library(sf)
library(osmdata)
library(sysfonts)
library(showtext)

# ------ Typography ------ 

font_add_google("Cardo", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Color palette ------ 

bg_color <- "#f9ece3"
shape_color <- "#fbf3d5"
road_color <- "#1a1a1a"
building_color <- "#5f6a86"
park_color <- "#a6b1a1"


# ------ Get Data ------ 

bbx <- getbb("Islamabad","," ,"PK")

# polygon shape
shape <- getbb("Islamabad","," ,"PK",  format_out = "sf_polygon")
shape<-shape[1,]

# Roads data
roads <- opq(bbx)|>
  add_osm_feature(key = "highway", 
                  value = c("motorway","trunk",
                            "primary", "secondary",
                            "tertiary", "motorway_link",
                            "trunk_link", "primary_link",
                            "secondary_link", "tertiary_link")) |>
  osmdata_sf()

# Get small streets
streets <- opq(bbx) |>
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",  "service",
                            "unclassified", "pedestrian", "footway",
                            "track","path")) |>
  osmdata_sf()

# Get parks
parks <- opq(bbx)|>
  add_osm_feature(key = "leisure", value = "park")|>
  osmdata_sf()

# Circle - Center

city_coords <- tibble(address = "Islamabad, PK")|>
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)


long <- city_coords$long
lat<- city_coords$lat

crs2 <- 24313 

center_proj <- tibble(lat=lat, long=long)|>
  st_as_sf(coords = c("long", "lat"), crs=4326)

# circle crop

distance <- 5500
circle <- tibble(lat=lat, long=long) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(crs = crs2) |>
  st_buffer(dist = distance) |>
  st_transform(crs = 4326)


# filter data
streets_lines <- st_intersection(circle, streets$osm_lines)
roads_lines <- st_intersection(circle, roads$osm_lines)
parks_polygons <- st_intersection(circle, parks$osm_polygons)



# ------ Map ------ 
ggplot()+
  geom_sf(data = streets_lines,
          col = road_color,
          size = .4,
          alpha = .65 ) +
  
  geom_sf(data = roads_lines,
          col = road_color,
          size = .6,
          alpha = .8) +
  geom_sf(data = parks_polygons,
          fill=park_color,
          color=park_color)+
  labs(
    caption = "<span style='font-size: 30px;margin-bottom: 35px;'>PAKISTAN</span><br><span style='font-size:85px;'>ISLAMABAD</span><br><br><span style='font-size:35px;'>33° 68' N - 73° 04' E</span><br><br>
       <span style='font-size:14px;'>Data : OpenStreetMap  **  Graphic: @imagineazhar</span>"
  ) +
  theme_void()+
  theme(
    # CAPTION
    plot.caption = ggtext::element_markdown(hjust = .5,
                                            family = title_font,
                                            face = "bold"), 
    plot.background = element_rect(fill=bg_color, color=NA),
    plot.margin = margin(b=20)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("08-osm.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)