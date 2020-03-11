# CREATE DATE: 3/10/2020
# LAST UPDATED: 3/10/2020

# THIS FILE IS TO SORT OUT CODY HERD SITUATION BY YEAR AND CREATE A GENERAL WINTER RANGE
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(adehabitatHR)

data.all <- read.csv("allWinterPoints_CodyHerd1990-2010.csv")
data.all$acquisition_time <- as.POSIXct(strptime(as.character(data.all$acquisition_time),"%Y-%m-%d %H:%M:%S"))
data.all$year <- as.numeric(format(data.all$acquisition_time, "%Y"))  
data.all$month <- as.numeric(format(data.all$acquisition_time, "%m"))  
data.all <- data.all[(!is.na(data.all$year) & !is.na(data.all$month)),]

data.animal.info <- data.all %>% group_by(year) %>% summarize (num_ind = n_distinct(gps_sensors_animals_id))

data.all <- data.all %>% mutate (group = (ifelse((year < 1997), "A", 
                                                 ifelse((year >2013), "C", "B"))))

# HR generation
target.crs <- "+proj=longlat +datum=WGS84"
xy <- cbind(data.all$longitude, data.all$latitude)
pts.sp.byyear <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data.all$year), proj4string = CRS(target.crs))
pts.sp.bygroup <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data.all$group), proj4string = CRS(target.crs))

kud.year <-  kernelUD(pts.sp.byyear, h = "href", grid = 500, same4all = TRUE)
kud.group <- kernelUD(pts.sp.bygroup, h = "href", grid = 500, same4all = TRUE)

image(kud)
