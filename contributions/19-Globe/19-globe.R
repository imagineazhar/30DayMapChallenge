library(tidyverse)
library(sf)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Inter", "title_font")
showtext_auto()
title_font <- "title_font"


# ------ Color palette ------ 

ocean_col <- "#B4CDE6"
land_col <- "#F5EFE6"
point_col <- "#FF731D"


# ------ Get Data ------ 

gisco_countries <- giscoR::gisco_countries

crs_string <- "+proj=ortho +lat_0=20 +lon_0=20 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = crs_string)

# meteorite landing
met_loc = read.csv("https://data.nasa.gov/api/views/gh4g-9sfh/rows.csv")
met_loc = met_loc[ , c("reclong", "reclat")]|> 
  na.omit()

met_sf <- st_as_sf(met_loc, coords = c("reclong", "reclat"),
                   crs=4326)

world <- gisco_countries|>
  st_intersection(ocean |> st_transform(4326))|> # select visible area
  st_transform(crs_string) # Ortho re-projection

# ------ Map ------ 

ggplot(world)+
  geom_sf(data=ocean, color=ocean_col, fill=ocean_col, linewidth=1)+
  geom_sf(fill = land_col, color = "#CFB997") +
  geom_sf(data=met_sf, color=point_col, alpha=0.6, size=1.5 )+
  labs(title = "Meteorite Landings",
       caption = "Data: NASA * Created by: M.Azhar")+
  theme_void() +
  theme(
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(family = title_font,
                              face = "bold",
                              color = "black",
                              size = 24,
                              lineheight = 1,
                              hjust = 0.5,
                              margin = margin(20,0,20,0)),
    # CAPTION
    plot.caption=element_text(color="#393E46",
                              family = title_font,
                              size=11,
                              hjust=0.5),
    plot.background = element_rect(fill = "#FAF7F0", color = NA),
    plot.margin = margin(20,20,20,20)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("19-globe-meteorite.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)