# CREATE DATE: 3/10/2020
# LAST UPDATED: 3/10/2020

# THIS FILE IS TO SORT OUT FOUR HERDS SITUATION BY YEAR AND CREATE A GENERAL WINTER RANGE
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(adehabitatHR)

# data.all <- read.csv("allWinterPoints_CodyHerd1990-2010.csv")
# data.new <- read.csv("sixMileTargheeJackson.csv")
# data.all <- rbind((data.all %>% dplyr::select(-X)), data.new)
# 
# #write.csv(data.all, "fourherds_all.csv")
# 
# # randomly select 20 individual-year from each herd to map HR
# data.all$acquisition_time <- as.POSIXct(strptime(as.character(data.all$acquisition_time),"%Y-%m-%d %H:%M:%S"))
# data.all$year <- as.numeric(format(data.all$acquisition_time, "%Y"))  
# data.all$AnimalYear <- paste0(data.all$gps_sensors_animals_id, "_", data.all$year)
# data.all <- data.all %>% filter(!is.na(year))
# 
# sample_n_groups = function(tbl, size, replace = FALSE, weight=NULL) {
#   # regroup when done
#   grps = tbl %>% groups %>% unlist %>% as.character
#   # check length of groups non-zero
#   keep = tbl %>% summarise() %>% sample_n(size, replace, weight)
#   # keep only selected groups, regroup because joins change count.
#   tbl %>% right_join(keep, by=grps) 
# }
# 
# set.seed(1777)
# data.selected <- data.all %>% group_by(herd, AnimalYear) %>% sample_n_groups(size = 20)
# 
# write.csv(data_selected, "FourHerds_20Ind.csv")
data.selected <- read.csv("FourHerds_20Ind.csv")
data.selected$acquisition_time <- as.POSIXct(strptime(as.character(data.selected$acquisition_time),"%Y-%m-%d %H:%M:%S"))
data.selected$month <- as.numeric(format(data.selected$acquisition_time, "%m"))  
data.selected <- data.selected %>% filter(!is.na(longitude) & !is.na(latitude) & !is.na(month))

#winter month
data.selected <- data.selected %>% filter(month %in% c(12,1,2,3))

# HR generation
target.crs <- "+proj=longlat +datum=WGS84"
xy <- cbind(data.selected$longitude, data.selected$latitude)
pts.sp.byherd <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data.selected$herd), proj4string = CRS(target.crs))

kud.herd <- kernelUD(pts.sp.byherd, h = "href", grid = 500, same4all = TRUE)
#image(kud.herd)
ver <- getverticeshr(kud.herd, 95)
plot(ver, col = 1:4)
#ver_50 <- getverticeshr(kud.herd, 50)
#plot(ver_50, col = 1:4)

proj.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84"
ver.projed <- spTransform(ver,crs(proj.crs))
ver.buffered <- gBuffer(ver.projed, byid = TRUE, id = ver.projed$id, 5000)
plot(ver.buffered, col = 1:4)
writeOGR(ver, getwd(), layer = "HR_90kud_4herds_20Ind", driver="ESRI Shapefile")
writeOGR(ver.buffered, getwd(), layer = "HR_90kud_4herds_20Ind_5km", driver="ESRI Shapefile")
