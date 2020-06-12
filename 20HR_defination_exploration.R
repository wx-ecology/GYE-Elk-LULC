# created: June 11, 2020
# this script is to explore different HR definations 
# part 1: different % of kernel density 
# part 2: different # of individuals
# part 3: different estimators 

## setup -----
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(adehabitatHR)
library(leaflet)

target.crs <- "+proj=longlat +datum=WGS84"

data.all <- read.csv("fourherds_all.csv")
data.all$acquisition_time <- as.POSIXct(strptime(as.character(data.all$acquisition_time),"%Y-%m-%d %H:%M:%S"))
data.all$year <- as.numeric(format(data.all$acquisition_time, "%Y"))
data.all$AnimalYear <- paste0(data.all$gps_sensors_animals_id, "_", data.all$year)
data.all <- data.all %>% filter(!is.na(year)) %>%  dplyr::select(-X)
data.all$month <- as.numeric(format(data.all$acquisition_time, "%m"))  

# check dataset summary
data.all %>% group_by(herd) %>% summarise( unique_ani_year = length(unique(AnimalYear)))
data.all %>% group_by(herd) %>% summarise( unique_ani_year = length(unique(year)))

# make sure no na data
data.all <- data.all %>% filter(!is.na(longitude) & !is.na(latitude) & !is.na(month))
# filter winter month only
data.all <- data.all %>% filter(month %in% c(12,1,2,3))

## function ----
sample_n_groups = function(tbl, size, replace = FALSE, weight=NULL) {
  # regroup when done
  grps = tbl %>% groups %>% unlist %>% as.character
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  tbl %>% right_join(keep, by=grps)
}

## pt1: % kernal density using 20 individual years ---
# use 20 individuals 
set.seed(1777)
data.selected <- data.all %>% group_by(herd, AnimalYear) %>% sample_n_groups(size = 20)

xy <- cbind(data.selected$longitude, data.selected$latitude)
data.selected.sp <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data.selected$herd), proj4string = CRS(target.crs))

# general kud
kud <- kernelUD(data.selected.sp, h = "href", grid = 500, same4all = TRUE)
ver95 <- getverticeshr(kud, 95)
ver95$kud <- "95"
ver90 <- getverticeshr(kud, 90)
ver90$kud <- "90"
ver75 <- getverticeshr(kud, 75)
ver75$kud <- "75"
ver50 <- getverticeshr(kud, 50)
ver50$kud <- "50"
ver25 <- getverticeshr(kud, 25)
ver25$kud <- "25"
ver10 <- getverticeshr(kud, 10)
ver10$kud <- "10"
ver <- rbind(ver95, ver90, ver75, ver50, ver25, ver10)

factpal <- colorFactor(palette = "PiYG", ver$kud)
leaflet(ver) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              color = ~factpal(kud)) %>%
  addLegend("bottomright", pal = factpal, values = ~kud,
            title = "HR by %kud",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
  )

## pt2: # of invidual used, 50 kud ----
diff_ind_HR <- function(fulldataframe = data.all, ind_num, HRperc = 50, grid = 500) {
  set.seed(1777)
  data <- fulldataframe %>% group_by(herd, AnimalYear) %>% sample_n_groups(size = ind_num)
  xy <- cbind(data$longitude, data$latitude)
  sp <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data$herd), proj4string = CRS(target.crs))
  kud <-  kernelUD(sp, h = "href", grid = grid, same4all = TRUE)
  ver <- getverticeshr(kud, HRperc)
  return(ver)
}

xy <- cbind(data.all$longitude, data.all$latitude)
sp.all <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data.all$herd), proj4string = CRS(target.crs))
kud.all <- kernelUD(sp.all, h = "href", grid = 500, same4all = TRUE)
ver.all <- getverticeshr(kud.all, 50)
ver.all$ind <- "all"
ver50 <- diff_ind_HR(ind_num=50)
ver50$ind <- "50"
ver40 <- diff_ind_HR(ind_num=40)
ver40$ind <- "40"
ver30 <- diff_ind_HR(ind_num=30)
ver30$ind <- "30"
ver20 <- diff_ind_HR(ind_num=20)
ver20$ind <- "20"
ver10 <- diff_ind_HR(ind_num=10)
ver10$ind <- "10"
ver_ind <- rbind(ver.all, ver50,ver40,ver30,ver20,ver10)

factpal <- colorFactor(palette = "BuPu", ver_ind$ind)
leaflet(ver_ind) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              color = ~factpal(ind)) %>%
  addLegend("bottomright", pal = factpal, values = ~ind,
            title = "50% Kernel HR by different individual #",
            opacity = 1
  )


## pt3, different selection of individuals, 50% KUD----
diff_set_ind <- function(fulldataframe = data.all, ind_num = 20, HRperc = 50, grid = 500) {
  data <- fulldataframe %>% group_by(herd, AnimalYear) %>% sample_n_groups(size = ind_num)
  xy <- cbind(data$longitude, data$latitude)
  sp <- SpatialPointsDataFrame (coords = xy, data = as.data.frame(data$herd), proj4string = CRS(target.crs))
  kud <-  kernelUD(sp, h = "href", grid = grid, same4all = TRUE)
  ver <- getverticeshr(kud, HRperc)
  return(ver)
}

ver20.a <- diff_set_ind(ind_num=20)
ver20.a$set <- "a"
ver20.b <- diff_set_ind(ind_num=20)
ver20.b$set <- "b"
ver20.c <- diff_set_ind(ind_num=20)
ver20.c$set <- "c"
ver20.d <- diff_set_ind(ind_num=20)
ver20.d$set <- "d"
ver20.e <- diff_set_ind(ind_num=20)
ver20.e$set <- "e"
ver_set_20 <- rbind(ver20.a, ver20.b, ver20.c, ver20.d, ver20.e)


ver30.a <- diff_set_ind(ind_num=30)
ver30.a$set <- "a"
ver30.b <- diff_set_ind(ind_num=30)
ver30.b$set <- "b"
ver30.c <- diff_set_ind(ind_num=30)
ver30.c$set <- "c"
ver30.d <- diff_set_ind(ind_num=30)
ver30.d$set <- "d"
ver30.e <- diff_set_ind(ind_num=30)
ver30.e$set <- "e"
ver_set_30 <- rbind(ver30.a, ver30.b, ver30.c, ver30.d, ver30.e)

ver40.a <- diff_set_ind(ind_num=40)
ver40.a$set <- "a"
ver40.b <- diff_set_ind(ind_num=40)
ver40.b$set <- "b"
ver40.c <- diff_set_ind(ind_num=40)
ver40.c$set <- "c"
ver40.d <- diff_set_ind(ind_num=40)
ver40.d$set <- "d"
ver40.e <- diff_set_ind(ind_num=40)
ver40.e$set <- "e"
ver_set_40 <- rbind(ver40.a, ver40.b, ver40.c, ver40.d, ver40.e)

ver50.a <- diff_set_ind(ind_num=50)
ver50.a$set <- "a"
ver50.b <- diff_set_ind(ind_num=50)
ver50.b$set <- "b"
ver50.c <- diff_set_ind(ind_num=50)
ver50.c$set <- "c"
ver50.d <- diff_set_ind(ind_num=50)
ver50.d$set <- "d"
ver50.e <- diff_set_ind(ind_num=50)
ver50.e$set <- "e"
ver_set_50 <- rbind(ver50.a, ver50.b, ver50.c, ver50.d, ver50.e)


pal <- colorFactor(palette = "Dark2", domain = ver_set_20$set)
leaflet() %>% addTiles() %>%
  addPolygons(data = ver_set_20, color = ~pal(ver_set_20$set),
              stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              group = "random 20 individuals"
  ) %>%
  addPolygons(data = ver_set_30, color = ~pal(ver_set_30$set),
              stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              group = "random 30 individuals"
  ) %>%
  addPolygons(data = ver_set_40, color = ~pal(ver_set_40$set),
              stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              group = "random 40 individuals"
  ) %>%
  addPolygons(data = ver_set_50, color = ~pal(ver_set_50$set),
              stroke = FALSE, smoothFactor = 0.2, 
              opacity = 1.0, fillOpacity = 0.5,
              group = "random 50 individuals"
  ) %>%
  addLayersControl(
    overlayGroups =c("random 20 individuals", "random 30 individuals", "random 40 individuals",  "random 50 individuals"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>% hideGroup("random 30 individuals") %>% hideGroup("random 40 individuals") %>% hideGroup("random 50 individuals")



# ## pt4: method 
# ### 50% kernel density w/ 20 ind
# par(mar=c(1,1,1,1))
# ver_kud <- ver50
# 
# ### fixed k LoCoH
# # he convex hull for each point is constructed from the (k-1) nearest neighbors to that point. 
# # Hulls are merged together from smallest to largest based on the area of the hull.
# ar <- LoCoH.k.area(data.selected.sp[,1], k = c(8:13))
# # pick a k where value starts to flatten out
# 
# ### 50% broanian bridge # give up - too much details 
# data.cody <- data.all %>% filter(herd == "Cody")
# traj.cody <- as.ltraj(xy =  cbind(data.cody$longitude, data.cody$latitude), data.cody$acquisition_time, id = as.character(data.cody$animals_code))
# dist <- vector()
# for (i in 1:length(traj.cody)) {
#   data <- traj.cody[[i]] %>% filter(!is.na(dist))
#   dist <- c(dist, data$dist)
# }
# sig2 <- sd(dist)
# liker(traj.cody, sig2 = sig2, rangesig1 = c(0, 0.001))
# tata <- kernelbb(traj.cody[1], sig1 = 0, sig2 = sig2, grid = 500)
# 
# 
