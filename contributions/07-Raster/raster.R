library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(giscoR)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Mukta", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Color palette ------ 

bg_color <- '#F2E7D5'

# ------ Get Data ------ 


crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

country <- gisco_get_countries(year = "2020",
                               epsg = "4326",
                               resolution = "10",
                               country = "Austria")


shape <- st_transform(country, crs = crsLONGLAT)

country_elevation <- get_elev_raster(locations = shape, 
                                     z = 7, 
                                     clip = "locations")
country_elev_df <- as.data.frame(country_elevation, xy=T)|>
  na.omit()

colnames(country_elev_df)[3] <- "elevation"


# ------ Map ------ 

ggplot()+
  geom_tile(data = country_elev_df, 
            aes(x = x, y = y, fill = elevation)) +
  marmap::scale_fill_etopo() +
  coord_sf(crs = crsLONGLAT)+
  labs( title = "Elevation Map: Austria",
      caption=" Graphic: @imagineazhar")+
  theme_void()+
  theme(
    legend.position = "none",
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 40,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,20,0)),
    # CAPTION
    plot.caption=element_text(color="#393E46",
                              family = title_font,
                              size=12,
                              hjust=0.5),
    plot.background = element_rect(fill=bg_color, color=NA),
    plot.margin = margin(20,20,20,20)
  )

# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("raster.png", height = 8,
       width = 10, dpi=320)  
showtext_auto(FALSE)