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
library(tidyr)

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

######################################################################
## bar plot # reflect a base info ##################################################
######################################################################
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

# figure out herd rankings in key LC
LCPRI_25ind.rank <- LCPRI_25ind.2 %>% group_by(Land_Cover, herd) %>% mutate(perc = round(area*100/total.area, digits = 2)) %>% arrange(by = Land_Cover, desc(perc))
View(LCPRI_25ind.rank)

############################################################
################ line plot ####################################################
############################################################

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
############################################################
# change plot 1985 vs 2017################################################################################
############################################################
LCPRI_25IND_DEV.rank <- LCPRI_25IND_DEV %>% filter(year %in% c(1985, 2017)) %>% group_by(herd) %>%
  select(herd, total.area, area, year) %>% pivot_wider(names_from = year, values_from = area) 
names(LCPRI_25IND_DEV.rank)[3:4] <- c("y1985", 'y2017')
LCPRI_25IND_DEV.rank <- LCPRI_25IND_DEV.rank %>% ungroup() %>% 
  mutate (r1985 = y1985/total.area, r2017 = y2017/total.area, rate = (y2017 - y1985)*100/y1985) %>% 
  mutate(mycolor = ifelse(rate > 0, "type1", "type2"), 
         herd = factor(herd, c("Blacktail", "Clarks Fork", "Cody", "Gooseberry", "Jackson", "Madison", "Northern", "North Madison", "Sand Creek", "Targhee", "Wiggins Fork"))) 

p3.1 <- ggplot(LCPRI_25IND_DEV.rank, aes (x = herd, y = rate)) +
  geom_segment( aes(x=herd, xend=herd, y=0, yend=rate, color=mycolor), size=1.3, alpha=0.9) +
  #geom_point( aes(x=herd, y=r1985*100), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=herd, y=rate), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_ipsum(base_size = 12, axis_title_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  theme(legend.position = "none") +
  labs(title = "Developed", x = element_blank(), y = element_blank()) +
  coord_flip() +
  ylim(-65, 50) 

LCPRI_25IND_AG.rank <- LCPRI_25IND_AG %>% filter(year %in% c(1985, 2017)) %>% group_by(herd) %>%
  select(herd, total.area, area, year) %>% pivot_wider(names_from = year, values_from = area) 
names(LCPRI_25IND_AG.rank)[3:4] <- c("y1985", 'y2017')
LCPRI_25IND_AG.rank <- LCPRI_25IND_AG.rank %>% ungroup() %>% 
  mutate (r1985 = y1985/total.area, r2017 = y2017/total.area, rate = (y2017 - y1985)*100/y1985) %>% 
  mutate(mycolor = ifelse(rate > 0, "type1", "type2") , 
         herd = factor(herd, c("Blacktail", "Clarks Fork", "Cody", "Gooseberry", "Jackson", "Madison", "Northern", "North Madison", "Sand Creek", "Targhee", "Wiggins Fork")))

p3.2 <- ggplot(LCPRI_25IND_AG.rank, aes (x = herd, y = rate)) +
  geom_segment( aes(x=herd, xend=herd, y=0, yend=rate, color=mycolor), size=1.3, alpha=0.9) +
  #geom_point( aes(x=herd, y=r1985*100), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=herd, y=rate), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_ipsum(base_size = 12, axis_title_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  theme(legend.position = "none") +
  labs(title = "Agriculture", x = element_blank(), y = element_blank()) +
  coord_flip() +
  ylim(-65, 50)


LCPRI_25IND_GRS.rank <- LCPRI_25IND_GRS %>% filter(year %in% c(1985, 2017)) %>% group_by(herd) %>%
  select(herd, total.area, area, year) %>% pivot_wider(names_from = year, values_from = area) 
names(LCPRI_25IND_GRS.rank)[3:4] <- c("y1985", 'y2017')
LCPRI_25IND_GRS.rank <- LCPRI_25IND_GRS.rank %>% ungroup() %>% 
  mutate (r1985 = y1985/total.area, r2017 = y2017/total.area, rate = (y2017 - y1985)*100/y1985) %>% 
  mutate(mycolor = ifelse(rate > 0, "type1", "type2") , 
         herd = factor(herd, c("Blacktail", "Clarks Fork", "Cody", "Gooseberry", "Jackson", "Madison", "Northern", "North Madison", "Sand Creek", "Targhee", "Wiggins Fork")))

p3.3 <- ggplot(LCPRI_25IND_GRS.rank, aes (x = herd, y = rate)) +
  geom_segment( aes(x=herd, xend=herd, y=0, yend=rate, color=mycolor), size=1.3, alpha=0.9) +
  #geom_point( aes(x=herd, y=r1985*100), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=herd, y=rate), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_ipsum(base_size = 12, axis_title_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  theme(legend.position = "none") +
  labs(title = "Grass/shrub", x = element_blank(), y = element_blank()) +
  coord_flip() +
  ylim(-65, 50)


LCPRI_25IND_TREE.rank <- LCPRI_25IND_TREE %>% filter(year %in% c(1985, 2017)) %>% group_by(herd) %>%
  select(herd, total.area, area, year) %>% pivot_wider(names_from = year, values_from = area) 
names(LCPRI_25IND_TREE.rank)[3:4] <- c("y1985", 'y2017')
LCPRI_25IND_TREE.rank <- LCPRI_25IND_TREE.rank %>% ungroup() %>% 
  mutate (r1985 = y1985/total.area, r2017 = y2017/total.area, rate = (y2017 - y1985)*100/y1985) %>% 
  mutate(mycolor = ifelse(rate > 0, "type1", "type2") , 
         herd = factor(herd, c("Blacktail", "Clarks Fork", "Cody", "Gooseberry", "Jackson", "Madison", "Northern", "North Madison", "Sand Creek", "Targhee", "Wiggins Fork")))

p3.4 <- ggplot(LCPRI_25IND_TREE.rank, aes (x = herd, y = rate)) +
  geom_segment( aes(x=herd, xend=herd, y=0, yend=rate, color=mycolor), size=1.3, alpha=0.9) +
  #geom_point( aes(x=herd, y=r1985*100), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=herd, y=rate), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme_ipsum(base_size = 12, axis_title_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  theme(legend.position = "none") +
  labs(title = "Tree Cover", x = element_blank(), y = element_blank()) +
  coord_flip() +
  ylim(-65, 50)

plot_grid(p3.1, p3.2, p3.3, p3.4)

##################################################
## stacked bar plot ####################################################
##################################################


