# calculating and visualizing LULC by herd
## input: LCMAP dataframe calculated for all herds 1985-2017 (25 ind and all ind)
####

library(raster)
library(animation)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggspatial)
library(hrbrthemes)
library(cowplot)

# data prep #
# csv data#
LCPRI_25ind <- read.csv("LCPRI_allHerds_25ind.csv")
# sp data #
HR_25ind_w_agency <- readOGR("AllHerd_25ind_w_Agency_clean.shp")
HR_25ind <- readOGR("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Elk\\Analysis\\GYE-Elk-LULC\\Data\\FINALwinterRangeSHP_June2020\\25elkYears\\allHerds25.shp")
target.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
HR_25ind <- spTransform(HR_25ind, target.crs)

## animation of LULC map ##
draw.a.map <- function (index) {
  raster.i <- raster(paste0(getwd(),"\\Data\\LCPRI_HR_25ind_1985-2017\\LCPRI_HR_25ind_", index, ".tif"))
  cuts=c(0,1,2,3,4,5,6,7) #set breaks
  pal <- colorRampPalette(c("#FF5D51", "#FDDC58", "#CAE9B0", "#3bbf4e", "#2E95D4", "#9FDAFF", "#F8FCFF", "#D1D1D1"))
  plot(raster.i, breaks=cuts, col = pal(8), axes=FALSE, main=paste0(index, " land cover")) 
  plot(HR_25ind, col = "#d1d1d150", lwd = 2, add = T)
}

years <-  c(1985:2005)

loop.animate <- function () {
  lapply(1:length(years), function(i) {
    draw.a.map(years[i])
  })
}

saveGIF(loop.animate(), interval = .2, movie.name = "LCMAP_allherds_25ind_V2.gif")

## bar plot # reflect a base info
## plot 1.1 ## pri-pub ratio across years for each herd
LCPRI_25ind.1 <- LCPRI_25ind %>% 
  group_by(herd, year, agency) %>% filter (year == 2000) %>% 
  mutate(total.area.by.agency = (sum(developed + cropland + grass_shrub + tree_cover + water + wetland + ice_snow + barren))*30*30/1000000)
LCPRI_25ind.11 <- LCPRI_25ind.1 %>% group_by(herd) %>% summarise(total.area = sum(total.area.by.agency))
LCPRI_25ind.1 <- LCPRI_25ind.1 %>% left_join(LCPRI_25ind.11, by = "herd") 
LCPRI_25ind.1 <- LCPRI_25ind.1 %>% mutate (percent = round( total.area.by.agency/total.area * 100, digits = 1)) %>% mutate (label = paste0(percent, "%"))
LCPRI_25ind.1[LCPRI_25ind.1$agency == "PUBLIC", ]$label <- NA

LCPRI_25ind.1$herd <- with(LCPRI_25ind.1, reorder(herd, total.area))
p1.1 <- ggplot(data = LCPRI_25ind.1, aes(x = herd, y = total.area.by.agency, fill = agency)) +
  geom_bar(position="stack", stat="identity") + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  theme(legend.position = c(0.2, 0.9)) + 
  theme(legend.title=element_blank()) +
  labs(title = "Elk herd winter home range area (km2)", x = element_blank(), y = element_blank()) +
  theme(plot.title = element_text(size=14))
p1.1
## plot 1.2 ## averaged % LULC across years for each herd
LCPRI_25ind.2 <- LCPRI_25ind %>% 
  tidyr::gather ("Land_Cover", "area", 4:11) %>%
  group_by(herd, year, Land_Cover) %>% summarise(area = sum(area)*30*30/1000000)
LCPRI_25ind.2 <- LCPRI_25ind.2 %>% group_by(herd, Land_Cover) %>% summarise(area = mean(area))
LCPRI_25ind.22 <- LCPRI_25ind.2 %>% group_by(herd) %>% summarise(total.area = sum(area))
LCPRI_25ind.2 <- LCPRI_25ind.2 %>% left_join(LCPRI_25ind.22, by = "herd")

LCPRI_25ind.2$herd <- with(LCPRI_25ind.2, reorder(herd, total.area))
LCPRI_25ind.2 <- LCPRI_25ind.2 %>% mutate(Land_Cover = fct_relevel(Land_Cover, "developed", "cropland", "grass_shrub", "tree_cover", "water", "wetland", "ice_snow", "barren"))
p1.2 <- ggplot(LCPRI_25ind.2, aes(x=herd, y = area, fill = Land_Cover)) +
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  scale_fill_manual(values = c("#FF5D51", "#FDDC58", "#CAE9B0", "#3bbf4e", "#2E95D4", "#9FDAFF", "#F8FCFF", "#D1D1D1")) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  theme(legend.title=element_blank()) +
  labs(title = "Averge LC area (km2) 1985-2007", x = element_blank(), y = element_blank()) +
  theme(plot.title = element_text(size=14))
p1.2

## line plot ##
LCPRI_25ind.3 <- LCPRI_25ind %>% group_by(herd, year) %>% 
  mutate(total.area = (sum(developed + cropland + grass_shrub + tree_cover + water + wetland + ice_snow + barren)))
## plot 2.1 # %HR that is developed over years for each herd, all ownership
  LCPRI_25IND_DEV <- LCPRI_25ind.3 %>% group_by(herd, year, total.area) %>% 
    summarise(area = (sum(developed))) %>% mutate(perc = area/total.area*100)
  p2.1 <- LCPRI_25IND_DEV %>%
    ggplot(aes(x = year, y=perc, group = herd, color = herd)) +
    geom_line(size = 1.2) +
    theme_ipsum() +
    scale_color_brewer(palette="Paired") +
    theme(legend.title=element_blank()) +
    labs(title = "Developed area (%HR)", x = element_blank(), y = element_blank()) +
    theme(plot.title = element_text(size=14))+ 
    theme(legend.position = "none")
  ## plot 2.2 # %HR that is cropland over years for each herd, all ownership
  LCPRI_25IND_AG <- LCPRI_25ind.3 %>% group_by(herd, year, total.area) %>% 
    summarise(area = (sum(cropland))) %>% mutate(perc = area/total.area*100)
  p2.2 <- LCPRI_25IND_AG %>%
    ggplot(aes(x = year, y=perc, group = herd, color = herd)) +
    geom_line(size = 1.2) +
    theme_ipsum() +
    scale_color_brewer(palette="Paired") +
    theme(legend.title=element_blank()) +
    labs(title = "Agriculture area (%HR)", x = element_blank(), y = element_blank()) +
    theme(plot.title = element_text(size=14))+ 
    theme(legend.position = "none")
  
  # if without Targhee
  # p2.2.noT <-LCPRI_25ind.3 %>% filter(herd != "Targhee") %>% group_by(herd, year, total.area) %>% 
  #   summarise(area = (sum(cropland))) %>% mutate(perc = area/total.area*100) %>%
  #   ggplot(aes(x = year, y=perc, group = herd, color = herd)) +
  #   geom_line(size = 1.2) +
  #   theme_ipsum() +
  #   scale_color_brewer(palette="Paired") +
  #   theme(legend.title=element_blank()) +
  #   labs(title = "Agriculture area (%HR)", x = element_blank(), y = element_blank()) +
  #   theme(plot.title = element_text(size=14))
  
  ## plot 2.3 # %HR that is tree over years for each herd, all ownership
  LCPRI_25IND_TREE <- LCPRI_25ind.3 %>% group_by(herd, year, total.area) %>% 
    summarise(area = (sum(tree_cover))) %>% mutate(perc = area/total.area*100)
  p2.3 <- LCPRI_25IND_TREE %>%
    ggplot(aes(x = year, y=perc, group = herd, color = herd)) +
    geom_line(size = 1.2) +
    theme_ipsum() +
    scale_color_brewer(palette="Paired") +
    theme(legend.title=element_blank()) +
    labs(title = "Tree cover (%HR)", x = element_blank(), y = element_blank()) +
    theme(plot.title = element_text(size=14))+ 
    theme(legend.position = "none")
  ## plot 2.4 # %HR that is grass_shrub years for each herd, all ownership
  LCPRI_25IND_GRS <- LCPRI_25ind.3 %>% group_by(herd, year, total.area) %>% 
    summarise(area = (sum(grass_shrub))) %>% mutate(perc = area/total.area*100)
  p2.4 <- LCPRI_25IND_GRS %>%
    ggplot(aes(x = year, y=perc, group = herd, color = herd)) +
    geom_line(size = 1.2) +
    theme_ipsum() +
    scale_color_brewer(palette="Paired") +
    theme(legend.title=element_blank()) +
    labs(title = "Grass/shrub area (%HR)", x = element_blank(), y = element_blank()) +
    theme(plot.title = element_text(size=14))+ 
    theme(legend.position = "none")
  
  plot_grid(p2.1, p2.2, p2.3, p2.4)

  
## who are the top 3 for each category??

## line plot ##
## plot 2.4 # %HR that is developed over years for each herd, pri ownership
## plot 2.5 # %HR that is cropland over years for each herd, pri ownership
## plot 2.6 # %HR that is tree over years for each herd, pri ownership
## does that change rankings of the herds??






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


