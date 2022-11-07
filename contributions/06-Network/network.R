library(tidyverse)
library(sf)
library(osmdata)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Squada One", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Color palette ------ 

bg_color <- '#16324F'
shape_color <- "#2A628F"
road_color <- '#46B3FF'
subway_color <- '#E2DCC8'
station_color <- "#F1D00A"

# ------ Get Data ------ 

# Area Coordinates
coords<-getbb("Staten Island")

# polygon shape
shape <- getbb("Staten Island, New York",  format_out = "sf_polygon")
shape<-shape[3,]

# track lines
subway <- coords |>
  opq() |>
  add_osm_feature(key = "railway", value = "subway") |>
  osmdata_sf() 

subway_data <- subway$osm_lines |> 
  filter(is.na(service))

# subway stops
stations <- coords |> 
  opq() |> 
  add_osm_feature(key = "public_transport", value = "station") |> 
  osmdata_sf()

stations_data <- stations$osm_points |> 
  filter(network == "Staten Island Railway")


# large roads
roads <- coords |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary",
                            "secondary", "tertiary", "motorway_link",
                            "trunk_link", "primary_link", "secondary_link",
                            "tertiary_link")) |>
  osmdata_sf()

roads<-roads$osm_lines|>
  st_intersection(shape)

# small roads
#streets <- coords |>
#  opq()|>
#  add_osm_feature(key = "highway",
#                  value = c("residential", "living_street",
#                            "service", "unclassified", "pedestrian",
#                            "footway", "track", "path"))|>
#  osmdata_sf()

#streets <- streets$osm_lines|>
#  st_intersection(shape)

# ------ Map ------ 

ggplot()+
  geom_sf(data = shape, color=road_color, fill=shape_color)+
  geom_sf(data = roads,
          inherit.aes = FALSE,
          color = road_color,
          size = .4)+
  geom_sf(data = subway_data,
          size = 2,
          colour = subway_color)+
  geom_sf(data = stations_data,
          size = 6,
          colour = station_color) +
  geom_sf(data = stations_data,
          size = 3,
          colour = "#3E6D9C") +
  geom_sf_text(data = stations_data,
               aes(label = str_wrap(name, 10)),
               size = 3.5,
               color="white",
               nudge_y = -0.0045,
               nudge_x = 0.004)+
  
  labs( title = "Staten Island Subway",
        caption=" Graphic: @imagineazhar | Data: {osmdata}")+
  theme_void()+
  theme(
    text=element_text(color="white"),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "white",
                              size = 40,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,20,0)),
    # CAPTION
    plot.caption=element_text(color=road_color,
                              family = title_font,
                              size=12,
                              hjust=0.5),
    plot.background = element_rect(fill=bg_color, color=NA),
    plot.margin = margin(20,20,20,20)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("06-network.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)