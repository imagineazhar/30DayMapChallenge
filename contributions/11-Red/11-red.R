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

bg_color <- "#d3e3df"
shape_color <- "#bc3535"


# ------ Get Data ------ 

bbx <- getbb("Islamabad","," ,"PK")


# polygon shape
shape <- getbb("Islamabad","," ,"PK",  format_out = "sf_polygon")
shape<-shape[1,]


city_coords <- tibble(address = "Islamabad, PK")|>
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)

long <- city_coords$long
lat<- city_coords$lat


# ------ Map ------ 

ggplot()+
  geom_sf(data = shape, fill=shape_color, color=shape_color)+
  geom_point(aes(x=long, y=lat), shape="\u2605", size=8, fill = "white")+
  geom_text(aes(x=long+.035, y=lat+0.015, label="Islamabad"), size=6)+
  
  labs( title = "<span style='font-size: 30px;margin-bottom: 35px;'>City of</span><br><br><span style='font-size:85px;'>ISLAMABAD</span><br><br><br><br><br>",
        caption="<span style='font-size:14px;'><br><br><br><br>Data : OpenStreetMap  **  Graphic: @imagineazhar</span>")+
  theme_void()+
  theme(
    # TITLE
    plot.title = ggtext::element_markdown(hjust = 0.5,
                                          family = title_font,
                                          face = "bold"),
    # CAPTION
    plot.caption = ggtext::element_markdown(hjust = .5,
                                            family = title_font,
                                            face = "bold"), 
    plot.background = element_rect(fill=bg_color, color=NA),
    plot.margin = margin(50,20,30,20)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("11-red.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)