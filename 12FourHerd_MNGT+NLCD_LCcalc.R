# calculating and visualizing LULC by herd
## input: 
#### polygon: land management agency 
#### polygon: 4 herd HR + 5km buffer
#### raster: NLCD every year

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(devtools)
library(cleangeo)
library(rasterVis)
library(ggplot2)
library(animation)
library(hrbrthemes)

elk_ranges <- readOGR("HR_90kud_4herds_20Ind_5km.shp")
target.crs <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

## color themes ##
agencycol <- c('#FFD37F', '#73DFFF', '#F0F0F0')
# from 1 to 8: water, developed, barren, forest, grass/shrub, pasture/cultivated, wetlands,ice/snow
lccol <- c('#1FBCFF', '#D85031', '#B3AC9F', '#418938', '#D3D000', '#83CE37', '#B184FF', '#67E3FF')

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

# for some reason the below code generate the intersect data with wrong lable. 
# So I took the SurfMngtAgency_AOI_clean.shp, in ArcGIS intersected with HR_90kud_4herds_20Ind_5km.shp, calculated area and then generated "HR_w_Agency.shp"

#   
# # Mngt_Agency <- readOGR("SurfMngtAgency_AOI_clean.shp")
# # Mngt_Agency <- Mngt_Agency[,2]
# # #spplot(Mngt_Agency, zcol = "Reclass")
# # #plot(elk_ranges, add = T)
# # fourherds_by_agency <- raster::intersect(elk_ranges, Mngt_Agency)
# # # plot(Mngt_Agency.1)
# # # plot(elk_ranges, add = T)
# # # plot(pi, add=T, col='yellow')
# # fourherds_by_agency$area = area(fourherds_by_agency)/1000000
# # fourherds_by_agency_df <- fourherds_by_agency@data
# # 
# # fourherds_by_agency_df.1 <- fourherds_by_agency_df %>% select(id,Reclass, area) %>%
# #   group_by(id, Reclass) %>% summarise(area = sum(area)) %>%
# #   group_by(id) %>% mutate(perc = area/sum(area)) %>% arrange(id)
# # fourherds_by_agency_df.1 <- fourherds_by_agency_df.1 %>% group_by(id) %>% mutate (ymax = cumsum(perc),
# #                                                                                   ymin = c(0, head(ymax, n=-1)),
# #                                                                                   labelposition = (ymax + ymin)/2)
# # 
# # ggplot(fourherds_by_agency_df.1, aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill=Reclass)) +
# #   geom_rect() +
# #   scale_fill_manual(values = agencycol) +
# #   scale_color_manual(values = '#969696') +
# #   coord_polar(theta="y") +
# #   xlim(c(-1, 6)) +
# #   theme_void() +
# #   #  theme(legend.position = "none") +
# #   facet_wrap(~id)

fourherds_by_agency <- readOGR("HR_w_Agency.shp")
fourherds_by_agency_df <- fourherds_by_agency@data

fourherds_by_agency_df.1 <- fourherds_by_agency_df %>% select(id,Reclass, area) %>%
  group_by(id, Reclass) %>% summarise(area = sum(area)) %>%
  group_by(id) %>% mutate(perc = area/sum(area)) %>% arrange(id)
fourherds_by_agency_df.1 <- fourherds_by_agency_df.1 %>% group_by(id) %>% mutate (ymax = cumsum(perc),
                                                                                  ymin = c(0, head(ymax, n=-1)),
                                                                                  labelposition = (ymax + ymin)/2)

ggplot(fourherds_by_agency_df.1, aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(Reclass, levels = c("FEDERAL", "STATE", "PRIVATE")))) +
  geom_rect() +
  scale_fill_manual(values = agencycol) +
  scale_color_manual(values = '#969696') +
  coord_polar(theta="y") +
  xlim(c(-1, 6)) +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~id) + 
  theme(strip.text  = element_text( size = 20))


##############################################################################################################################
#################### calculating four herds NLCD every year by agency ########################################################
##############################################################################################################################

# lsvis <- function(r) {
#   r[r>8] <- NA
#   return(r)
# }

# create data feame ##################################
filenames <- list.files("FourHerd_LC_NLCD/", pattern = "*\\.tif$")
files <- lapply(paste0("FourHerd_LC_NLCD/", filenames), raster)
#files <- lapply(files,lsvis)
#levelplot(files[[1]], col.regions = lccol, att = 'ID')

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

NLCD.df <- cbind(NLCD.df[,10:11], NLCD.df[,9], NLCD.df[,1:8])
names(NLCD.df) <- c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")
# write.csv(NLCD.df, "NLCD_4herd_w_agency.csv")
# note, values are numbers of pixels but not areas


# clean the dataframe ################################
NLCD.1 <- NLCD.df %>% group_by(herd, agency, year) %>% summarise(water = sum(water),
                                                                 developed = sum(developed),
                                                                 barren = sum(barren),
                                                                 forest = sum(forest),
                                                                 shrub_grass = sum(shrub_grass),
                                                                 cultivated_pasture = sum(cultivated_pasture),
                                                                 wetlands = sum(wetlands),
                                                                 ice_snow = sum(ice_snow))


NLCD.total <- NLCD.1 %>% tidyr::gather("LC", "Count", 4:11) %>% group_by(year, herd, LC) %>% summarise(Count = sum(Count)) %>%
  group_by(year, herd) %>% mutate(LCperc = Count/sum(Count),
                                  ymax = cumsum(LCperc),
                                  ymin = c(0, head(ymax, n=-1)))


# year 2001, LC donut plot ######################
ggplot(NLCD.total %>% filter(year == '2001'), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
  geom_rect() +
  scale_fill_manual(values = lccol) +
  scale_color_manual(values = '#969696') +
  coord_polar(theta="y") +
  xlim(c(-1, 6)) +
  theme_void() +
  theme(legend.position = "none") +
#  theme(legend.title = element_blank()) +
  facet_wrap(~herd) + 
  theme(strip.text  = element_text( size = 20))

# ALL ownerships ALL categoies, animation #####################
draw.a.plot <- function (index) {
  g <- ggplot(NLCD.total %>% filter(year == index), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
    geom_rect() +
    scale_fill_manual(values = lccol) +
    scale_color_manual(values = '#969696') +
    coord_polar(theta="y") +
    xlim(c(-1, 6)) +
    theme_void() +
    theme(legend.position = "none") +
    #  theme(legend.title = element_blank()) +
    facet_wrap(~herd) + 
    theme(strip.text  = element_text( size = 20)) +
    ggtitle(index) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  print(g)
}

years <-  c("2001", "2004", "2006", "2008", "2011", "2013", "2016")

loop.animate <- function () {
  lapply(1:length(years), function(i) {
    draw.a.plot(years[i])
  })
}

saveGIF(loop.animate(), interval = .5, movie.name = "NLCD_4herds_total.gif")

# BY OWNERSHIPS - PRIVATE ONLY OVER YEARS ANIMATION AND LINE PLOTS ########################
NLCD.pri <- NLCD.1 %>% filter (agency == 'PRIVATE')%>% tidyr::gather("LC", "Count", 4:11) %>% group_by(year, herd, LC) %>% summarise(Count = sum(Count)) %>%
  group_by(year, herd) %>% mutate(LCperc = Count/sum(Count),
                                  ymax = cumsum(LCperc),
                                  ymin = c(0, head(ymax, n=-1)))

ggplot(NLCD.pri %>% filter(year == '2001'), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
  geom_rect() +
  scale_fill_manual(values = lccol) +
  scale_color_manual(values = '#969696') +
  coord_polar(theta="y") +
  xlim(c(-1, 6)) +
  theme_void() +
  theme(legend.position = "none") +
  #  theme(legend.title = element_blank()) +
  facet_wrap(~herd) + 
  theme(strip.text  = element_text( size = 20))

# ANIMATIO
draw.a.plot <- function (index) {
  g <- ggplot(NLCD.pri %>% filter(year == index), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
    geom_rect() +
    scale_fill_manual(values = lccol) +
    scale_color_manual(values = '#969696') +
    coord_polar(theta="y") +
    xlim(c(-1, 6)) +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap(~herd) + 
    theme(strip.text  = element_text( size = 20)) +
    ggtitle(index) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  print(g)
}

years <-  c("2001", "2004", "2006", "2008", "2011", "2013", "2016")

loop.animate <- function () {
  lapply(1:length(years), function(i) {
    draw.a.plot(years[i])
  })
}

saveGIF(loop.animate(), interval = .5, movie.name = "NLCD_4herds_PRIVATE.gif")

# LINE PLOT
ggplot(data = NLCD.pri, aes (x = year, y = LCperc, group = LC, color = factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = lccol) +
  facet_wrap(~herd) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

# BY OWNERSHIPS - FEDERAL ONLY OVER YEARS ANIMATION AND LINE PLOTS ######################
NLCD.fed <- NLCD.1 %>% filter (agency == 'FEDERAL')%>% tidyr::gather("LC", "Count", 4:11) %>% group_by(year, herd, LC) %>% summarise(Count = sum(Count)) %>%
  group_by(year, herd) %>% mutate(LCperc = Count/sum(Count),
                                  ymax = cumsum(LCperc),
                                  ymin = c(0, head(ymax, n=-1)))

draw.a.plot <- function (index) {
  g <- ggplot(NLCD.fed %>% filter(year == index), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
    geom_rect() +
    scale_fill_manual(values = lccol) +
    scale_color_manual(values = '#969696') +
    coord_polar(theta="y") +
    xlim(c(-1, 6)) +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap(~herd) + 
    theme(strip.text  = element_text( size = 20)) +
    ggtitle(index) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  print(g)
}

years <-  c("2001", "2004", "2006", "2008", "2011", "2013", "2016")

loop.animate <- function () {
  lapply(1:length(years), function(i) {
    draw.a.plot(years[i])
  })
}

saveGIF(loop.animate(), interval = .5, movie.name = "NLCD_4herds_FEDERAL.gif")

ggplot(data = NLCD.fed, aes (x = year, y = LCperc, group = LC, color = factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = lccol) +
  facet_wrap(~herd) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

# BY OWNERSHIPS - PUBLIC ONLY OVER YEARS ANIMATION AND LINE PLOTS ######################
NLCD.pub <- NLCD.1 %>% filter ((agency == 'FEDERAL') | (agency == 'STATE') )%>% tidyr::gather("LC", "Count", 4:11) %>% group_by(year, herd, LC) %>% summarise(Count = sum(Count)) %>%
  group_by(year, herd) %>% mutate(LCperc = Count/sum(Count),
                                  ymax = cumsum(LCperc),
                                  ymin = c(0, head(ymax, n=-1)))

draw.a.plot <- function (index) {
  g <- ggplot(NLCD.pub %>% filter(year == index), aes(ymax=ymax, ymin=ymin, xmax=6, xmin=3, fill= factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
    geom_rect() +
    scale_fill_manual(values = lccol) +
    scale_color_manual(values = '#969696') +
    coord_polar(theta="y") +
    xlim(c(-1, 6)) +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap(~herd) + 
    theme(strip.text  = element_text( size = 20)) +
    ggtitle(index) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  print(g)
}

years <-  c("2001", "2004", "2006", "2008", "2011", "2013", "2016")

loop.animate <- function () {
  lapply(1:length(years), function(i) {
    draw.a.plot(years[i])
  })
}

saveGIF(loop.animate(), interval = .5, movie.name = "NLCD_4herds_PUBLIC.gif")

# line plot
ggplot(data = NLCD.pub, aes (x = year, y = LCperc, group = LC, color = factor(LC, levels = c( "herd", "agency", "year", "water", "developed", "barren", "forest", "shrub_grass", "cultivated_pasture", "wetlands", "ice_snow")))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = lccol) +
  facet_wrap(~herd) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

# By land use type data frame prep ###############
total <- NLCD.1 %>% tidyr::gather("LC", "total", 4:11) %>% group_by(herd, year, LC) %>% summarise(total = sum(total)) 
area.sum <- total %>% group_by(herd, year) %>% summarise(total = sum(total))
# BY LAND USE TYPES - developed only ###############
dev <- total %>% group_by(herd, year) %>% filter(LC == "developed") %>% left_join(area.sum, by = c("herd", "year")) %>% mutate (perc = total.x/total.y)
ggplot(data = dev, aes (x = as.numeric(as.character(year)), y = perc*100, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Developed land (as percentage of all land types)", x = "year", y = "percent") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

ggplot(data = dev, aes (x = as.numeric(as.character(year)), y = total.x*30*30/1000000, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Developed land", x = "year", y = "km2") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 
# BY LAND USE TYPES - agriculture only #############
ag <- total %>% group_by(herd, year) %>% filter(LC == "cultivated_pasture") %>% left_join(area.sum, by = c("herd", "year")) %>% mutate (perc = total.x/total.y)
ggplot(data = ag, aes (x = as.numeric(as.character(year)), y = perc*100, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Agriculture land (as percentage of all land types)", x = "year", y = "percent") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

ggplot(data = ag, aes (x = as.numeric(as.character(year)), y = total.x*30*30/1000000, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Agriculture land", x = "year", y = "km2") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 
# BY LAND USE TYPES - forest only #############
forest <- total %>% group_by(herd, year) %>% filter(LC == "forest") %>% left_join(area.sum, by = c("herd", "year")) %>% mutate (perc = total.x/total.y)
ggplot(data = forest, aes (x = as.numeric(as.character(year)), y = perc*100, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Forest land (as percentage of all land types)", x = "year", y = "percent") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

ggplot(data = forest, aes (x = as.numeric(as.character(year)), y = total.x*30*30/1000000, group = herd, color = factor(herd))) +
  geom_line(size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  theme_ipsum() +
  labs(title = "Forest land", x = "year", y = "km2") +
  theme(plot.title = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) 

##################################################################################################
############### LC change matrix/hotspot########## ###############################################
##################################################################################################

# did I just used HR to clip NLCD?

filenames <- list.files("FourHerd_LC_NLCD/", pattern = "*\\.tif$")
files <- lapply(paste0("FourHerd_LC_NLCD/", filenames), raster)

s <- stack(files)
#levelplot(s, col.regions = lccol, att = 'ID')
s.1 <- s[[1]]
s.2 <- s[[2]]
s.3 <- s[[3]]
s.4 <- s[[4]]
s.5 <- s[[5]]
s.6 <- s[[6]]
s.7 <- s[[7]]


### LU change hotspot regardless of class ########################
## function for detecting any class change to any class
LCchange <- function(r2, r1) {
  r.12 = r2 - r1
  r.12[r.12 <0] <- 1 
  r.12[r.12 >0] <- 1
  return (r.12)
} 

# levelplot(s.1)

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
writeRaster(hotspot, "FourHerds_ChangeHotspot.tif", format = "GTiff")

##### forest lost hotspot and by year #############################
## function for detecting forest loss
FrLoss <- function(r2, r1) {
  #forest-non-forest: 1-0
  r1[r1 != 4] <- 0
  r1[r1 == 4] <- 1
  r2[r2 != 4] <- 0
  r2[r2 == 4] <- 1
  # calculate differences
  r.12 = r2 - r1
  r.12[r.12 != -1] <- 0  #if -1, mean r2=0, r1=1, representing forest loss
  r.12[r.12 == -1] <- 1 
  return (r.12) #return raster where forest is lost = 1, else 0
} 

f.12 <- overlay(s.2, s.1, fun = FrLoss) # t1 first t0 second (later year first)
f.23 <- overlay(s.3, s.2, fun = FrLoss)
f.34 <- overlay(s.4, s.3, fun = FrLoss)
f.45 <- overlay(s.5, s.4, fun = FrLoss)
f.56 <- overlay(s.6, s.5, fun = FrLoss)
f.67 <- overlay(s.7, s.6, fun = FrLoss)
f <- stack(f.12, f.23, f.34, f.45, f.56, f.67)

# first creat empty raster 
sum.f <- f[[1]] # start from a taster with all cells being 0
for (i in 2:6) {
  if (i > 1) {
    mask = sum.f
    mask[mask != 0] <- 1
    f[[i]] <- mask(f[[i]], mask = mask, maskvalue = 1, updatevalue = 0)
    f[[i]][f[[i]] == 1] <- i
    sum.f = f[[i]] + sum.f
    }
}
writeRaster(sum.f, "FourHerds_ForestLoss_byYear.tif", format = "GTiff")


##### Ag increase hotspot by year ################################
## function for detecting forest loss
AgInc <- function(r2, r1) {
  #forest-non-forest: 1-0
  r1[r1 != 6] <- 0
  r1[r1 == 6] <- 1
  r2[r2 != 6] <- 0
  r2[r2 == 6] <- 1
  # calculate differences
  r.12 = r2 - r1
  r.12[r.12 != 1] <- 0  #if 1, means r2=1, r1=0, representing ag increase
  return (r.12) #return raster where forest is lost = 1, else 0
} 

a.12 <- overlay(s.2, s.1, fun = AgInc) # t1 first t0 second (later year first)
a.23 <- overlay(s.3, s.2, fun = AgInc)
a.34 <- overlay(s.4, s.3, fun = AgInc)
a.45 <- overlay(s.5, s.4, fun = AgInc)
a.56 <- overlay(s.6, s.5, fun = AgInc)
a.67 <- overlay(s.7, s.6, fun = AgInc)
a <- stack(a.12, a.23, a.34, a.45, a.56, a.67)

# first creat empty raster 
sum.a <- a[[1]] # start from a taster with all cells being 0
for (i in 2:6) {
  if (i > 1) {
    mask = sum.a
    mask[mask != 0] <- 1
    a[[i]] <- mask(a[[i]], mask = mask, maskvalue = 1, updatevalue = 0)
    a[[i]][a[[i]] == 1] <- i
    sum.a = a[[i]] + sum.a
  }
}
writeRaster(sum.a, "FourHerds_AgIncrease_byYear.tif", format = "GTiff")

##### Dev increase hotspot by year ################################
## function for detecting forest loss
DevInc <- function(r2, r1) {
  #forest-non-forest: 1-0
  r1[r1 != 2] <- 0
  r1[r1 == 2] <- 1
  r2[r2 != 2] <- 0
  r2[r2 == 2] <- 1
  # calculate differences
  r.12 = r2 - r1
  r.12[r.12 != 1] <- 0  #if 1, means r2=1, r1=0, representing ag increase
  return (r.12) #return raster where forest is lost = 1, else 0
} 

d.12 <- overlay(s.2, s.1, fun = DevInc) # t1 first t0 second (later year first)
d.23 <- overlay(s.3, s.2, fun = DevInc)
d.34 <- overlay(s.4, s.3, fun = DevInc)
d.45 <- overlay(s.5, s.4, fun = DevInc)
d.56 <- overlay(s.6, s.5, fun = DevInc)
d.67 <- overlay(s.7, s.6, fun = DevInc)
d <- stack(d.12, d.23, d.34, d.45, d.56, d.67)

# first creat empty raster 
sum.d <- d[[1]] # start from a taster with all cells being 0
for (i in 2:6) {
  if (i > 1) {
    mask = sum.d
    mask[mask != 0] <- 1
    d[[i]] <- mask(d[[i]], mask = mask, maskvalue = 1, updatevalue = 0)
    d[[i]][d[[i]] == 1] <- i
    sum.d = d[[i]] + sum.d
  }
}
writeRaster(sum.d, "FourHerds_DevIncrease_byYear.tif", format = "GTiff")


