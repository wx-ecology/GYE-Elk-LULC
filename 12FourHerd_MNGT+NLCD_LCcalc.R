# calculating and visualizing LULC by herd
## input: 
#### polygon: land management agency 
#### polygon: 4 herd HR + 5km buffer

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(devtools)
library(cleangeo)
library(rasterVis)

elk_ranges <- readOGR("HR_90kud_4herds_20Ind_5km.shp")
target.crs <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

# ###### clean Mngt agency shp ##########
# Mngt_Agency <- readOGR("SpatialMidProcess/SurfMngtAgency_AOI.shp")
# Mngt_Agency <- spTransform(Mngt_Agency, target.crs)
# Mngt_Agency.1 <- Mngt_Agency[,c(8,13)]
# # fix Mngt orphoned hole issue 
# report <- clgeo_CollectionReport(Mngt_Agency.1)
# summary <- clgeo_SummaryReport(report)
# issues <- report[report$valid == FALSE,]
# nv <- clgeo_SuspiciousFeatures(report)
# 
# #try to clean data
# mysp.clean <- clgeo_Clean(Mngt_Agency.1)
# #check if they are still errors
# report.clean <- clgeo_CollectionReport(mysp.clean)
# summary.clean <- clgeo_SummaryReport(report.clean)
# Mngt_Agency.1 <- mysp.clean
# writeOGR(Mngt_Agency.1, "SurfMngtAgency_AOI_clean.shp", "Surf Mngt Agency", driver = 'ESRI Shapefile')
# ######################################

###### calculating four herds agency overlap ##########
Mngt_Agency <- readOGR("SurfMngtAgency_AOI_clean.shp")
Mngt_Agency <- Mngt_Agency[,2]
fourherds_by_agency <- raster::intersect(elk_ranges, Mngt_Agency)
# plot(Mngt_Agency.1)
# plot(elk_ranges, add = T)
# plot(pi, add=T, col='yellow')
fourherds_by_agency$area = area(fourherds_by_agency)/1000000
fourherds_by_agency_df <- aggregate(area~ id + Reclass, data = fourherds_by_agency, FUN = sum)

###### calculating four herds NLCD overlap ##########
#set breaks
# from 1 to 8: water, developed, barren, forest, grass/shrub, pasture/cultivated, wetlands,ice/snow
pal <- c('#FFFFFF', '#1FBCFF', '#D85031', '#B3AC9F', '#418938', '#D3D000', '#83CE37', '#B184FF', '#67E3FF')

# lsvis <- function(r) {
#   r[r>8] <- NA
#   return(r)
# }

filenames <- list.files("FourHerd_LC_NLCD/", pattern = "*\\.tif$")
files <- lapply(paste0("FourHerd_LC_NLCD/", filenames), raster)
files <- lapply(files,lsvis)
#levelplot(files[[1]], col.regions = pal, att = 'ID')

fourherds.reproj <- spTransform(fourherds_by_agency, crs(NLCD))

# extract pixel by polygon and calculate counts for each value
countpixel <- function(x, ...){
  v <- c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6), sum(x==7), sum(x==8))
  return(v)
}

NLCD.df <- data.frame()
for (i in 1:length(files)) {
  NLCD <- files[[i]]
  cal <- as.data.frame(raster::extract(NLCD, fourherds.reproj, fun = countpixel))
  year <- substr(names(NLCD), 5, 8)
  cal$year <- year
  cal <- cbind(cal, herd = fourherds.reproj$id, agency = fourherds.reproj$Reclass)
  NLCD.df <- rbind(NLCD.df, cal)
}

names(NLCD.df) <- c("water", "developed", "barren", "forest", "shrub/grass", "cultivated/pasture", "wetlands", "ice/snow", "year", "herd", "agency")
NLCD.df <- cbind(NLCD.df[,10:11], NLCD.df[,9], NLCD.df[,1:8])
#write.csv(NLCD.df, "NLCD_4herd_w_agency.csv")



#### LC change matrix #####
# N1 <- raster("FourHerd_LC_NLCD/projection_wrong/FOURherds_NLCD_2001.tif")
# N2 <- raster("FourHerd_LC_NLCD/projection_wrong/FOURherds_NLCD_2004.tif")
# 
# s <- stack(NLCD1, NLCD2)
# levelplot(s, col.regions = pal, att = 'ID')

LCchange <- function(r2, r1) {
  r.12 = r2 - r1
  r.12[r.12 <0] <- 1 
  r.12[r.12 >0] <- 1
  return (r.12)
} 


levelplot(s.1)

s.1 <- s[[1]]
s.2 <- s[[2]]
s.3 <- s[[3]]
s.4 <- s[[4]]
s.5 <- s[[5]]
s.6 <- s[[6]]
s.7 <- s[[7]]

r.12 <- overlay(s.2, s.1, fun = LCchange)
r.23 <- overlay(s.3, s.2, fun = LCchange)
r.34 <- overlay(s.4, s.3, fun = LCchange)
r.45 <- overlay(s.5, s.4, fun = LCchange)
r.56 <- overlay(s.6, s.5, fun = LCchange)
r.67 <- overlay(s.7, s.6, fun = LCchange)

r <- stack(r.12, r.23, r.34, r.45, r.56, r.67)
levelplot(r)

hotspot <- sum(r)
levelplot(hotspot)
