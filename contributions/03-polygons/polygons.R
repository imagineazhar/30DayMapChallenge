library(tidyverse)
library(ggtext)
library(ggrepel)
library(osmdata)
library(sf)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Squada One", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Get Data ------ 

#get coord plot for Staten Island
coords<-getbb("London")

#get polygon shape
shape <- getbb("london",  format_out = "sf_polygon")
shape<-shape[1,]

#gather main streets
streets <- coords|>
  opq()|>
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) |>
  osmdata_sf()

#eliminate streets outside of london shape
streets<-streets$osm_lines|>
  st_intersection(shape)


leisure <- coords|>
  opq()|>
  add_osm_feature(key = "leisure") |>
  osmdata_sf()

leisure_poly<-leisure$osm_polygons|>
  st_intersection(shape)


# ------ Color Pallet ------ 
bg_color<-'#16324F'
shape_color<-'#2A628F'
road_color<-'#46B3FF'
parks<-'#54B66F'

# ------ Map ------ 

ggplot()+
  geom_sf(data=shape, color=road_color, fill=shape_color)+
  geom_sf(data = streets,
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


