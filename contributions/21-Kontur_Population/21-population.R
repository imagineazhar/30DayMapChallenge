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

# Download from https://data.humdata.org/dataset/kontur-population-dataset
pop <- read_sf(("data/kontur_population.gpkg"),
               query = "select * from population where population >= 1000")

gisco_countries <- giscoR::gisco_countries

crs_string <- "+proj=ortho +lat_0=30 +lon_0=70 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = crs_string)

pop_ortho <- pop |>
  st_transform(4326) |>
  st_intersection(ocean |>st_transform(4326)) |>
  st_transform(crs = crs_string)


world <- gisco_countries|>
  st_intersection(ocean |> st_transform(4326))|> # select visible area
  st_transform(crs_string) # Ortho re-projection

# ------ Map ------ 

ggplot(world) +
  geom_sf(data = ocean, fill = ocean_col, color = ocean_col) +
  geom_sf(data = pop_ortho, aes(color = population), linewidth = 0.1) +
  scale_color_distiller(palette = "OrRd", trans = "pseudo_log") +
  labs(
    caption = "Source: Kontur Population ** Created by: Muhammad Azhar"
  ) +
  theme_void() +
  theme(
    legend.position = "None",
    
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
ggsave("21-population.png", height = 10,
       width = 8, dpi=320)  
showtext_auto(FALSE)