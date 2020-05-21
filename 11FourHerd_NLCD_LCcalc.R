# Cody LULC 1992 - 2016 based on NLCD data in wintering area 
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(gridExtra)

# read earth engine output into one dataframe
multmerge <- function(mypath) {
  filenames = list.files( path=mypath, pattern="*.csv", full.names=TRUE )
  datalist = lapply( filenames, function(x) {
    data = read.csv(file=x, header=T)
    data = data %>% select(id, groups)
    data$year = strsplit(strsplit(x, "_")[[1]][5], "[.]")[[1]][1] 
    
    data$groups = lapply(unlist(data$groups), function(x) {strsplit(as.character(x), "[}][,] [{]")})
    groups = lapply(data$groups, function(x) {unlist(x)})
    for (i in (1:length(groups))) { 
      length(groups[i][[1]]) <- 8 
    }
    groups <- as.data.frame(groups, col.names = data$id) %>% tidyr::gather(id, LC)
    groups <- groups %>% tidyr::separate(LC, c("landcover", "lcarea"), sep = "[,] ")
    groups$landcover <- str_remove(groups$landcover, "landcover=")
    groups$landcover <- str_remove(groups$landcover, "\\[\\{")
    groups$lcarea <- str_remove(groups$lcarea, "sum=")
    groups$lcarea <- str_remove(groups$lcarea, "\\}\\]")
    
    data <- data %>% select(id, year) %>% right_join(groups, by = "id")
    for (i in unique(data$id)) {
      if (length(setdiff(c("1", "2", "3", "4", "5", "6", "7", "8"),data[data$id==i,]$landcover)) != 0) {
        data[data$id==i, ]$landcover[which(is.na(data[data$id==i,]$landcover))] <- setdiff(c("1", "2", "3", "4", "5", "6", "7", "8"),data[data$id==i,]$landcover)
      }
    }
    
    return(data)
    } )
  do.call(rbind.data.frame, datalist)
  }

dir <- paste0(getwd(), "/FourHerd_LC_NLCD")
data <- multmerge(dir)

#add landcover names 
LCnames <- read.csv("LCnames.csv")
LCnames$landcover <- as.character(LCnames$landcover)
data <- data %>% left_join(LCnames, by = "landcover")
data$year <- factor(data$year, levels = c("2001", "2004", "2006", "2008", "2011", "2013", "2016"))
data$lcarea <- as.numeric(data$lcarea)
data[is.na(data$lcarea),]$lcarea <- 0

#plot all areas together with selected land cover types
p1 <- ggplot(data %>% filter(LC_name == "agriculture") , aes(y=lcarea/1000000, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("agriculture")
p2 <- ggplot(data %>% filter(LC_name == "developed") , aes(y=lcarea/1000000, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("developed")
p3 <- ggplot(data %>% filter(LC_name == "forest") , aes(y=lcarea/1000000, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("forest")
grid.arrange(p1, p2, p3, nrow = 1)

#plot percentage
data.1 <- data %>% group_by(id, year) %>% mutate (total.area = sum(lcarea)) %>% mutate (lcperc = lcarea/total.area)
p11 <- ggplot(data.1 %>% filter(LC_name == "agriculture") , aes(y=lcperc*100, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("agriculture prec")
p12 <- ggplot(data.1 %>% filter(LC_name == "developed") , aes(y=lcperc*100, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("developed prec")
p13 <- ggplot(data.1 %>% filter(LC_name == "forest") , aes(y=lcperc*100, x=id, group = year)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum() +
  ggtitle("forest prec")
grid.arrange(p11, p12, p13, nrow = 1)



p21 <- ggplot(data.1 %>% filter(year == "2001") , aes(y=lcperc*100, x=id, group = landcover, fill = LC_name)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_ipsum() +
  ggtitle("agriculture prec 2001")
p22 <- ggplot(data.1 %>% filter(year == "2016") , aes(y=lcperc*100, x=id, group = landcover, fill = LC_name)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_ipsum() +
  ggtitle("agriculture prec 2016")
grid.arrange(p21, p22, nrow = 1)
