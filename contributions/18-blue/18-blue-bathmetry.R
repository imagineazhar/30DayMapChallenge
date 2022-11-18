library(marmap)
library(tidyverse)
library(sf)
library(rgeoboundaries)
library(sysfonts)
library(showtext)


# ------ Typography ------ 

font_add_google("Mukta", "title_font")
showtext_auto()

title_font <- "title_font"


# ------ Color palette ------ 

palette <- colorRampPalette(c("#03045e", "#0077b6", "#00b4d8",
                              "#90e0ef", "#caf0f8", "#caf0f8"))

# ------ Get Data ------ 

pak <- geoboundaries("Pakistan")
ira <- geoboundaries("Iran")
uae <- geoboundaries("United Arab Emirates")
bah <- geoboundaries("Bahrain")
qtr <- geoboundaries("Qatar")
omn <- geoboundaries("Oman")
ind <- geoboundaries("India")
afg <- geoboundaries("Afghanistan")


countries <- rbind(pak, ira, uae, bah, qtr, omn, ind, afg)

bathmetry <- getNOAA.bathy(lat1 = 30, lat2 = 20, lon1 = 49, lon2 = 70, resolution = 1)

df <- fortify.bathy(bathmetry)|>
  filter(z < 0)

# ------ Map ------ 

ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = z)) +
  geom_sf(data = countries, fill = "#FFFBF8", color = "grey50", size = 0.25)+
  coord_sf(ylim = c(30, 20), xlim = c(56, 73), expand = FALSE) +
  annotate("text", 70, 20.4, label= "Data: NOAA · Map: Azhar",
           family = title_font, size = 5, color = "#0353a4")+
  annotate("text", 59.3, 29, label= "Ocean Depth, Pakistan",
           family = title_font, size = 6, color = "#0353a4")+
  scale_fill_gradientn(colors = palette(10),
                       labels = function(x) format(-x, big.mark = " ", trim = TRUE)) +
  guides(fill = guide_colorbar(label.position = "left", title = "Depth (m)")) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.height = unit(2.5, "line"),
    legend.key.width = unit(0.75, "line"),
    plot.background = element_rect(fill = "#FFFBF8", color = NA)
    )

# ------ Save Plot ------ 

showtext_opts(dpi = 320) 
ggsave("18-bathmetry.png", height = 6,
       width = 11, dpi=320)  
showtext_auto(FALSE)