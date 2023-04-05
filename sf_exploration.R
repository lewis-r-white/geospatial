library(sf)
library(spData)
library(tmap)
library(dplyr)


lnd_point <- st_point(c(0.1, 51.5))
lnd_geom <- st_sfc(lnd_point, crs = 4326)   #simple feature geometry column. crs (coordinate reference system)   4326 corresponds to a coordinate reference system (WGS 84, specifically)

lnd_attrib <- data.frame(name = "London",
                         temperature = 25, 
                         date = as.Date("2022-10-05")
                         )

lnd_sf <- st_sf(lnd_attrib, geometry = lnd_geom)
lnd_sf



class(world) #sf allows you to leverage different ways of representing data. "data tables"
dim(world)
head(world)

summary(world$lifeExp)


test <- world |> 
  select(-geom)

colnames(test) # hard to get rid of the geometry haha




world_df <- st_drop_geometry(world) #getting rid of geometry and making it not sf class
class(world_df) #normal data frame 


world |> 
  select(name_long, pop)


world1 <- world |> 
  filter(area_km2 < 10000)

summary(world1$area_km2)



world2 <- world |> 
  filter(lifeExp >= 80)

tm_shape(world2) +
  tm_fill()


world %>%
  filter(continent == "Asia") |> 
  select(name_long, continent, lifeExp) %>%
  slice_max(lifeExp)


continents <- world |> 
  group_by(continent) |> 
  summarise(population = sum(pop, na.rm = TRUE))

tm_shape(continents) + 
  tm_polygons(col = "population",
              style = "cont")


world |> 
  group_by(continent) |> 
  summarise(population = sum(pop, na.rm = TRUE),
            area = sum(area_km2, na.rm = TRUE),
            n_countries = n_distinct(name_long)) |> 
  mutate(density = round(population/area)) |> 
  slice_max(density, n = 3) |> 
  arrange(desc(n_countries))


head(coffee_data)

world_coffee <- left_join(world, coffee_data, by = "name_long")
names(world_coffee)            

tm_shape(world_coffee) + 
  tm_polygons(col = "coffee_production_2017") +
  tm_layout(legend.outside = TRUE)


world_coffee_inner <- inner_join(world, coffee_data)

nrow(world_coffee_inner) 
nrow(coffee_data)

setdiff(coffee_data$name_long, world$name_long)


drc = stringr::str_subset(world$name_long, "Congo")

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] <- "Democratic Republic of the Congo"

##grepl


world_coffee_inner2 <- inner_join(world, coffee_data)
nrow(world_coffee_inner2)


tm_shape(world_coffee_inner2) + 
  tm_polygons(col = "coffee_production_2017") +
  tm_layout(legend.outside = TRUE)


