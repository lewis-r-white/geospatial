library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)


#set up data as file path? Can pass it directly into function

raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")

my_rast <- rast(raster_filepath)

my_rast  #says number of rows, columns and layers. resolution (this one is high res) ~ always in resolution of coordinate reference system.  extent. coordinate reference system. 

plot(my_rast)

tm_shape(my_rast) +
  tm_raster()


elev <- rast(nrows = 6, ncols = 6, #dimensions
             resolution = 0.5,  #resolution
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,  #extent    default is lon/lat WGS 84
             vals = 1:36) #values

plot(elev)

multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")

multi_rast <- rast(multi_raster_file)

multi_rast #resolution looks different bc were now in projected coordinate reference system
            #extent looks different ~ now in meters
            #names is showing the different bands (shows min and max of each band)
            #each band is a layer. stacked matrices. raster stacks. 

nlyr(multi_rast) # 4 layers

multi_rast3 <- subset(multi_rast, 3)  #just need to specify which layer we want from the data. 
nlyr(multi_rast3)#just 1 layer now

multi_rast4 <- subset(multi_rast, "landsat_4")  #another way to subset is by calling the name directly!
nlyr(multi_rast4) #just 1 layer now

plot(multi_rast)



# CATEGORICAL DATA 

grain_order <- c("clay", "silt", "sand")
grain_char <- sample(grain_order, 36, replace = TRUE)
grain_fact <- factor(grain_char, levels = grain_order)

grain <- rast(nrow = 6, ncol = 6,
              resolution = 0.5,
              xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
              vals = grain_fact)

#grid cell and pixel are used interchangeably!
plot(grain)



elev[1, 1]  #sub-setting cells
elev[36] #cell id
elev[1,]

two_layer <- c(grain, elev)

two_layer[1] #shows first cell for both layers 

elev[1,1] = 0 #updating raster by manually overriding 

two_layer[1] <- cbind(c(1), c(4)) #column bind two vectors (needs the cbind and the c())

two_layer[1]


summary(elev)

global(elev, sd)

freq(grain)   #tells us number of grid cells for each value 

hist(elev)





# DAY 2 RASTER

plot(elev)

id <- cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol =2))  #extracting the cell value for the long/lat(xy) coordinates of (0.1, 0.1)
id

elev[id]

terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))



clip <- rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45, 
             resolution = 0.3,
             vals = rep(1, 9))   #makes a clip / subset of a specific area.  repeating value 1 nine times. 

plot(clip)

test <- elev[clip] 
test #returns values within the clip range


class(test) # clip didn't give us back a raster, but just gave us the values that fell inside of clip
class(clip)

terra::extract(elev, ext(clip))  #returns values within the clip range


elev[1:2, drop = FALSE] #this is saying don't drop the geometry info, keep it as a raster!


rmask <- elev  #setting up rmask so it has the same extent, resolution etc as elev

values(rmask) <- sample(c(NA, TRUE), 36, replace= TRUE) #replacing the elevation raster values with mask values that are either NA or true. 

plot(rmask) #plotting the new mask raster


new_rast <- mask(elev, rmask)  #applying our mask raster to the elev data
plot(new_rast)


#new_raster <- elev[rmask, drop = FALSE]#didn't work?
#plot(new_raster)#didn't work?

elev[elev < 20] = NA #this is changing elev so all values below 20 are NA
plot(elev)



#adding in the normal elevation raster data again 

elev <- rast(nrows = 6, ncols = 6, #dimensions
             resolution = 0.5,  #resolution
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,  #extent    default is lon/lat WGS 84
             vals = 1:36) #values



#LOCAL ALGEBRA 

elev * 2  #since the raster data is matrix, easy to multiply 
plot(log(elev))
elev > 5


rcl <- matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE) #0-12 is turned into 1, 12-24 turned into 2, 24-36 turned into 3. 

recl <- classify(elev, rcl = rcl)
recl
plot(recl)

multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)


multi_rast


ndvi_fun <- function(nir, red){
  (nir - red) / (nir + red)
}

ndvi_rast <- lapp(multi_rast[[c(3, 4)]], fun = ndvi_fun)   #lapp is a more computationally efficient way to match things up. Landsat bands 3 and 4 correspond to Red and NIR.

plot(ndvi_rast)



# FOCAL ALGEBRA
elev <- rast(system.file("raster/elev.tif", package = "spData"))

r_focal <- focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min) #middle square is reference. the 9 squares around it (inclusive) is the area of interest. We're taking the minimum of those 9 squares and applying it to our square of interest. 

plot(r_focal)




#----------------------- NEW DAY -----------------------------

#load elev raster
elev <- rast(system.file("raster/elev.tif",
                         package = "spData"))
#define soil grain types
grain_order <- c("clay", "silt", "sand")
#randomly create soil layer
grain_char <- sample(grain_order, 36, replace = TRUE)
#convert to a factor
grain_fact <- factor(grain_char, levels = grain_order)

#turn into raster
grain <- rast(nrows = 6, ncols = 6, resolution = 0.5,
              xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
              vals = grain_fact)

plot(grain)
plot(elev)

#ZONAL ALGEBRA

zonal(elev, grain, fun = "mean")

#extends elev by 1 row on each side and two columns on each side
elev_2 <- extend(elev, c(1, 2))

plot(elev_2)

#elev + elev_2     this doens't work anymore bc extents don't match 

#i want elevation to match extent of elev 2. #adding NAs to force elev to match extent of elev 2
elev_3 <- extend(elev, elev_2)


#change origin
origin(elev_3)
origin(elev_3) <- c(0.25, 0.25)
origin(elev_3)



##  AGGREGATION AND DISAGGREGATION

#Faster datasets can also differ in their resolution to match resolutions we can decrease the resolution by aggregating or increase the resolution by disaggregating.

#Let's start by changing the resolution of a DEM by a factor of 5, by taking the mean.

dem <- rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg <-  aggregate(dem, fact = 5, fun = mean)

plot(dem)
plot(dem_agg)


#We have some choices when increasing the resolution. Here we try the bilinear method.

dem_disagg <- disagg(dem_agg, fact = 5, method = "bilinear")
identical(dem, dem_disagg)

plot(dem_disagg)



#RESAMPLING

#Aggregation/disaggregation work when both rasters have the same origins. What do we do in the case where we have two or more rasters with different origins and resolutions? Resampling computes values for new pixel locations based on custom resolutions and origins.

#In most cases, the target raster would be an object you are already working with, but here we define a target raster.

target_rast <- rast(xmin = 794600, xmax = 798200,
                    ymin = 8931800, ymax = 8935400,
                    resolution = 150, crs = "EPSG:32717")

dem_resampl <- resample(dem, y = target_rast, method = "bilinear")

plot(dem)
plot(dem_resampl)
 
