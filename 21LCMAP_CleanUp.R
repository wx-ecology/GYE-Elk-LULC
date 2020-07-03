# this is to mosaic LCMAP data and clip them with HR data
library(raster)
library(rgdal)

setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Elk/data_backup")
# # same crs as LCMAP data
target.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# read in shapefiles
AOI <- readOGR("General_AOI.shp")
AOI <- spTransform(AOI, target.crs)
elk_HR_25ind <- readOGR("C:/Users/wenjing.xu/Google Drive/RESEARCH/Elk/Analysis/GYE-Elk-LULC/AllHerd_25ind_w_Agency_clean.shp")
elk_HR_25ind <- spTransform(elk_HR_25ind, target.crs)



###############################################################
############ cleaning LCMAP data ##############################
###############################################################

#### mosaic data together #####################################
# 
# mosaicLCMAP <- function(year) {
#   dir.i <- paste0(getwd(),"/LCMAP/", year, "/")
#   filenames <- list.files(dir.i, pattern = "*\\LCPRI.tif$", recursive = TRUE) #only read the primary land cover files for the year
#   files <- lapply(paste0(dir.i, filenames), raster)
#   files$fun <- mean
#   rast.mosaic <- do.call(mosaic,files)
#   writeRaster(rast.mosaic, paste0(getwd(),"/LCMAP/LCMAP_LCPRI_", year), format = "GTiff")
# }
# 
# # for (i in 1985:2017) {
# #   mosaicLCMAP(i)
# # }

# ############# clip data into for each HR #######################
# ### conducting in ArcGIS is a bit easier #######################
# setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Elk\\Analysis\\GYE-Elk-LULC")
# LCMAP.dir <- paste0(getwd(), "/Data/LCPRI_Mosaic_GYE_1985-2017/")
# filenames <- list.files(LCMAP.dir, pattern = "*\\.tif$")
# LCPRI <- lapply(paste0(LCMAP.dir, filenames), raster)
# LCPRI_HR <- lapply(LCPRI, crop, y=elk_HR_25ind)
# LCPRI_HR <- lapply(LCPRI, mask, y=elk_HR_25ind)
# # save the rasters
# writeRaster.LCMAP <- function(x) {
#   name <- names(x)
#   name <- substr(name, 7,10)
#   writeRaster(x, filename = paste0(getwd(), "/Data/LCPRI_HR_25ind_1985-2017/LCPRI_HR_25ind_", name), format ="GTiff")
# }
# # lapply(LCPRI_HR, writeRaster.LCMAP) # cannot allocate RAM, try for loop
# for (i in 10:length(LCPRI_HR)) {
#   LCPRI_HR.i <- LCPRI_HR[[i]]
#   writeRaster.LCMAP(LCPRI_HR.i)
# }

########################################################################
############ start here for recalculation ##############################
########################################################################

############ calculate LCLU in each herd winter HR #####################

LCMAP.dir <- paste0(getwd(), "/Data/LCPRI_HR_25ind_1985-2017/")
LCPRI_HR <-  list.files(LCMAP.dir, pattern = "*\\.tif$")

countpixel <- function(x, ...){
  v <- c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6), sum(x==7), sum(x==8))
  return(v)
}

LCPRI.df <- data.frame()
for (i in 1:length(LCPRI_HR)) {
  LCPRI_HR.i <- raster(paste0(LCMAP.dir,LCPRI_HR[[i]]))
  cal <- as.data.frame(raster::extract(LCPRI_HR.i, elk_HR_25ind, fun = countpixel))
  year <- substr(names(LCPRI_HR.i), 16, 19)
  cal$year <- year
  cal <- cbind(cal, herd = elk_HR_25ind$id, agency = elk_HR_25ind$AGENCY)
  LCPRI.df <- rbind(LCPRI.df, cal)
}

LCPRI.df <- cbind(LCPRI.df[,10:11], LCPRI.df[,9], LCPRI.df[,1:8])
names(LCPRI.df) <- c( "herd", "agency", "year", "developed", "cropland", "grass_shrub", "tree_cover", "water", "wetland", "ice_snow", "barrern")
# LCPRI.df.25ind <- LCPRI.df
# write.csv(LCPRI.df, "LCPRI_allHerds_25ind.csv")


## horizontal bar plot # reflect a base info
## plot 1.1 ## pri-pub ratio across years for each herd
## plot 1.2 ## averaged % LULC across years for each herd

## line plot ##
## plot 2.1 # %HR that is developed over years for each herd, all ownership
## plot 2.2 # %HR that is cropland over years for each herd, all ownership
## plot 2.3 # %HR that is tree over years for each herd, all ownership
## who are the top 3 for each category??

## line plot ##
## plot 2.4 # %HR that is developed over years for each herd, pri ownership
## plot 2.5 # %HR that is cropland over years for each herd, pri ownership
## plot 2.6 # %HR that is tree over years for each herd, pri ownership
## does that change rankings of the herds??


