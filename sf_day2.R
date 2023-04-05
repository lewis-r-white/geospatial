library(sf)
library(tmap)
library(spData)
library(tidyverse)
library(rmapshaper)
library(smoothr)

nz
nz_height

canterbury <- nz |> 
  filter(Name == "Canterbury") 


#SPATIAL SUBSETTING

#option 1 for filtering data
c_height <- nz_height[canterbury,]

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(nz_height) +
  tm_dots(col = "red")

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(c_height) +
  tm_dots(col = "red")

outside_height <- nz_height[canterbury, , op = st_disjoint]

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(outside_height) +
  tm_dots(col = "red")


# option 2
sel_sgbp <- st_intersects(x = nz_height, y = canterbury)
sel_logical <- lengths(sel_sgbp) > 0
c_height_2 <- nz_height[sel_logical, ]

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(c_height_2) +
  tm_dots(col = "blue")

#option 3 for filtering data

c_height_3 <- nz_height %>%
  st_filter(y = canterbury, .predicate = st_intersects)

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(c_height_3) +
  tm_dots(col = "magenta")


outside_height_v2 <- nz_height %>%
  st_filter(y = canterbury, .predicate = st_disjoint)

tm_shape(nz) + 
  tm_polygons() +
  tm_shape(outside_height_v2) +
  tm_dots(col = "darkorchid")


### SPATIAL JOINING

bb <- st_bbox(world)  #creates box around the world


#creating a dataframe with 10 random points of longitude and 10 random points of latitude
random_df <- data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)

#converting these points of long/lat into an sf class with the info as a geometry column
random_points <- random_df %>%
  st_as_sf(coords = c("x", "y")) %>%  #specifying what the coords are
  st_set_crs("EPSG:4326")  #setting the desired coordinate reference system

class(random_points) #it's now an sf object!

tm_shape(world) +
  tm_fill() +
  tm_shape(random_points) +
  tm_dots(col = "red")


world_random <- world[random_points, ]

tm_shape(world_random) +
  tm_fill()




##NEW DAY

bb <- st_bbox(world)

random_df <- data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)


random_points <- random_df %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs("EPSG:4326")

world_random <- world[random_points, ]

tm_shape(world) +
  tm_polygons() +
  tm_shape(random_points) +
  tm_dots()

tm_shape(world_random) +
  tm_polygons()


random_joined <- st_join(random_points, world)   #st_join automatically uses spatial geometry as the join. Automatically left joins as well

random_joined_inner <- st_join(random_points, world, left = FALSE)  # keeps just the ones that have a match. The ocean points don't have a match, so this code only returns points on land. 




?cycle_hire

tmap_mode("view")

tm_shape(cycle_hire) + 
  tm_dots(col = "blue", alpha = 0.4) +
  tm_shape(cycle_hire_osm) +
  tm_dots(col = "red", alpha = 0.4)

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE)) #any asks if a condition is true for any point. This is askign if there are any overlapping points in these two datasets

sel <- st_is_within_distance(cycle_hire, cycle_hire_osm, dist = 20) #each point that is within 20 meters will be TRUE, others will be FALSE

summary(lengths(sel) > 0)    #304 points not within 20 meters. 438 points within 20 meters. 

close_joins <- st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, dist = 20)  #joining spatially if within 20 meters rather than by an exact match

nrow(cycle_hire)
nrow(close_joins) #this has more rows because some points joined to multiple others, creating duplicates. 


z <- close_joins %>%
  group_by(id) %>%
  summarise(capacity = mean(capacity))


nz_agg <- aggregate(x = nz_height, by = nz, FUN = mean) #average elevation within each region of NZ

nz_agg_tidy_version <- st_join(nz, nz_height) %>%
  group_by(Name) %>%
  summarise(elevation = mean(elevation, na.rm = TRUE))


## AREA-WEIGHTED INTERPOLATION
head(incongruent)
head(aggregating_zones)


tm_shape(incongruent) +
  tm_polygons() +
  tm_shape(aggregating_zones) +
  tm_borders(col = "red")


#simplify to cut down on computation time
iv <- incongruent["value"]  #get just the value column

agg_aw <- st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)  #aggregating on non shared spaces

#some variables are extensive (e.g. population distributed across whole area. A percentage is NOT an extensive variable. Can't do area weighted interpolation on non-extensive. )


tm_shape(agg_aw) + 
  tm_fill(col = "value")




nz_highest <- nz_height %>%
  slice_max(n = 1, order_by = elevation)

canterbury <- nz %>%
  filter(Name == "Canterbury")

canterbury_centroid <- st_centroid(canterbury)

st_distance(nz_height, canterbury_centroid)



###  SIMPLIFYING US STATES USING PUECKER ALGORITHM

us_states2163 <- st_transform(us_states, "EPSG:2163")

us_states_simple <- st_simplify(us_states2163,
                                dTolerance = 1000) #dTolerance is specifying epsilon in meters

tm_shape(us_states_simple) +
  tm_polygons()


us_state_simple2 <- rmapshaper::ms_simplify(us_states2163,
                                           keep = 0.01,         #percentage of points to keep
                                           keep_shapes = TRUE)

tm_shape(us_state_simple2) +
  tm_polygons()


#we transformed CRS because we wanted equal area, optimized for US in 2D. 


