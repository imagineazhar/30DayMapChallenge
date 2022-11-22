library(tidyverse)
library(sf)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Courgette", "title_font")
showtext_auto()
title_font <- "title_font"


# ------ Color palette ------ 

ocean_col <- "#F7F7F7"
land_col <- "#6D9886"
border_col <- "#F2E7D5"


# ------ Get Data ------ 

gisco_countries <- giscoR::gisco_countries

crs_string <- "+proj=ortho +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = crs_string)


world <- gisco_countries|>
  st_intersection(ocean |> st_transform(4326))|> # select visible area
  st_transform(crs_string) # Ortho re-projection

# ------ Map ------ 

ggplot(world)+
  geom_sf(data=ocean, color="grey50", fill=ocean_col, size=1)+
  geom_sf(fill = land_col, color = "NA")+
  geom_point(aes(x=0, y=0, size=1),
             color="black", shape=21, alpha=0.8)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  labs(title = "The NULL Island",
       subtitle = "Null Island is the point on the Earth's surface at\n zero degrees latitude and zero degrees longitude.",
       caption = "Created by: Azhar | Twitter: @imagineazhar")+
  theme_void() +
  theme(
    legend.position = "None",
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 26,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,10,0)),
    # SUB-TITLE
    plot.subtitle =  element_text(family = title_font,
                              face = "plain",
                              color = "grey60",
                              size = 12,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(0,0,10,0)),
    # CAPTION
    plot.caption=element_text(color="grey60",
                              family = title_font,
                              size=11,
                              hjust=0.5,
                              margin = margin(10,0,0,0)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0,20,20,20)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("22-null-island.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)