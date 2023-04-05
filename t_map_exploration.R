library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)


nz #vector data
#attributes associated with entire state

tm_shape(nz) +
  tm_fill()

tm_shape(nz) +
  tm_borders()

tm_shape(nz) +
  tm_fill() +
  tm_borders()

map_nz <- tm_shape(nz) +
  tm_polygons() #shortcut for  tm_fill() + tm_borders() 

class(map_nz)

#vector is correcter, raster is faster

nz_elev #raster dataset

#taking some obs, within Certain dimensions, what is average value in that cell. One value for that entire cell. For this oeach

map_nz +
  tm_shape(nz_elev) +
  tm_raster(alpha = 0.7)

nz_height #vector dataset 

map_nz1 <- map_nz +
  tm_shape(nz_elev) +
  tm_raster(alpha = 0.7)

map_nz1 +
  tm_shape(nz_height) +
  tm_dots()

#ggplot(data = x, aes(col = age))
#tmap does NOT use aes. can use aes directly. 

tm_shape(nz) +
  tm_fill(col = "steelblue")

tm_shape(nz) +
  tm_fill(col = "red",
          alpha = 0.2) +
  tm_borders(col = "steelblue",
             lwd = 2,             #line width
             lty = 2)           #lty is line type


legend_title <- expression("Area (km"^2*")")  

tm_shape(nz) +
  tm_fill(col = "Land_area",
          title = legend_title) +   #since the legend is determined by the color here, we sopecify the title here. 
  tm_borders()


tm_shape(nz) +
  tm_polygons(col = "Median_income",
              style = "pretty",  #style changes spacing of bins in legend. Pretty does nice whole numbers. Equal deivides them into equal shapes.
              palette = "viridis") 


# tmaptools::palette_explorer()   to check out palette options


breaks <- c(0, 3, 4, 5) * 10000

tm_shape(nz) +
  tm_polygons(col = "Median_income", breaks = breaks)


map_nz +
  tm_shape(nz_elev) +
  tm_raster(alpha = 0.7,
            style = "cont")

map_nz +
  tm_shape(nz) +
  tm_polygons(col = "Island",
              style = "cat")


map_nz +
  tm_compass(type = "4star", 
             position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200))


map_nz + 
  tm_graticules() +
  tm_layout(bg.color = "skyblue")


tmap_mode("view")
map_nz

tmap_mode("plot")
map_nz
