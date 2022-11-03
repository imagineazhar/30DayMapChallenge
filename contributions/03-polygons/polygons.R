library(tidyverse)
library(sf)
library(ggrepel)
library(osmdata)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Squada One", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Color palette ------ 

bg_color<-'#16324F'
shape_color<-'#2A628F'
road_color<-'#46B3FF'
parks<-'#54B66F'


# ------ Get Data ------ 

# Coordinates  for London
coords<-getbb("London")

# Select London Polygon
shape <- getbb("london",  format_out = "sf_polygon")
shape<-shape[1,]

# Roads data
roads <- coords|>
  opq()|>
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) |>
  osmdata_sf()

# Filter Road for London
roads<-roads$osm_lines|>
  st_intersection(shape)

# get leisure areas
leisure <- coords|>
  opq()|>
  add_osm_feature(key = "leisure") |>
  osmdata_sf()

leisure_poly<-leisure$osm_polygons|>
  st_intersection(shape)


# ------ Map ------ 

ggplot()+
  geom_sf(data=shape, color=road_color, fill=shape_color)+
  geom_sf(data = roads,
          inherit.aes = FALSE,
          color = road_color,
          size = .4)+
  geom_sf(data=leisure_poly|>filter(leisure %in% c("park","golf_course")),
          fill=parks,
          color=shape_color)+
  labs( title = "Parks & Golf Courses in London",
    caption="Data: OpenStreetMap & Google | Graphic: @imagineazhar")+
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
    plot.caption=element_text(color=road_color, hjust=0.05),
    plot.background = element_rect(fill=bg_color, color=NA),
    plot.margin = margin(20,20,20,20)
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("02-polygons.png", height = 8,
       width = 10, dpi=320)  
showtext_auto(FALSE)