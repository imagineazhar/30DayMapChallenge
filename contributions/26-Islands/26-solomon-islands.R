library(tidyverse)
library(sf)
library(osmdata)
library(sysfonts)
library(showtext)

# ------ Typography ------ 

font_add_google("Syne Mono", "title_font")
showtext_auto()

title_font <- "title_font"



# ------ Color palette ------ 

bg_col <- "light blue"
land_col <- "#6D9886"
border_col <- "#F7F7F7"


# ------ Get Data ------ 

crs_string <- "+proj=ortho +lat_0=10 +lon_0=160 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

gisco_countries <- giscoR::gisco_countries

island <- gisco_countries|>
  filter(ISO3_CODE == "SLB")

island_sf <- island$geometry


ocean <- st_point(x = c(0, 0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = crs_string)


# ------ Map ------ 

ggplot(island_sf)+
  geom_sf(color=border_col, fill=land_col, size=.8)+
  geom_sf(color=border_col, fill=NA, size=1)+
  geom_text(aes(x=165.7, y=-10.6, label="Lata"), family=title_font,size=3)+
  geom_text(aes(x=161.8, y=-8.91, label="Malaita"), family=title_font)+
  geom_text(aes(x=161.76, y=-10.94, label="San Cristobal"), family=title_font)+
  geom_text(aes(x=159.964, y=-9.44, label="Honiara"), family=title_font)+
  geom_text(aes(x=159.833, y=-9.104, label="Kusini"), family=title_font, size=3)+
  geom_text(aes(x=156.73, y=-8.1, label="Gizo"), family=title_font, size=3)+
  geom_text(aes(x=160.01, y=-11.31, label="Kangua"), family=title_font, size=3)+
  geom_text(aes(x=156.93, y=-7.05, label="Choiseul"), family=title_font, size=3.5)+
  geom_text(aes(x=159.62, y=-8.17, label="Buala"), family=title_font, size=3.5)+
  geom_text(aes(x=166.5, y=-11.2, label="Apakhö"), family=title_font, size=3.5)+
  geom_text(aes(x=155.6, y=-7.4, label="Mono Island"), family=title_font, size=4)+
  coord_sf(expand = TRUE)+
  coord_sf(expand = TRUE)+
  labs(title = "Solomon Islands",
       caption = "Graphic: Azhar | @imagineazhar")+
  theme_void()+
  theme(
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 24,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,0,0)),
    # CAPTION
    plot.caption=element_text(color="#393E46",
                              family = title_font,
                              size=11,
                              hjust=0.5,
                              margin = margin(10,0,0,0)),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(10,10,10,10)
  )

# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("26-Solomon-islands.png", height = 8,
       width = 10, dpi=320)  
showtext_auto(FALSE)


