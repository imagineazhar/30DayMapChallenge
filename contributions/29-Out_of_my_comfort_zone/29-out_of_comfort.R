library(rayshader)



# ------ Get Data ------ 

#Here, I load a map with the raster package.
localtif = raster::raster("dem_01.tif")

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat|>
  sphere_shade(texture = "desert") |>
  add_water(detect_water(elmat), color = "desert")|>
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45,
          windowsize = c(1000, 800))


Sys.sleep(0.2)
render_snapshot()
