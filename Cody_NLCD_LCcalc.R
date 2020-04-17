# Cody LULC 1992 - 2016 based on NLCD data in wintering area 

library(dplyr)
library(ggplot2)
library(hrbrthemes)

# read earth engine output into one dataframe
multmerge <- function(mypath) {
  filenames = list.files( path=mypath, pattern="*.csv", full.names=TRUE )
  datalist = lapply( filenames, function(x) {
    data = read.csv(file=x, header=T)
    data$year = strsplit(strsplit(x, "_")[[1]][5], "[.]")[[1]][1] 
    return(data)
    } )
  do.call(rbind.data.frame, datalist)
  }

dir <- paste0(getwd(), "/Cody_LC_NLCD")
data <- multmerge(dir)

data <- data %>% select(landcover, sum, year)

#add landcover names 
LCnames <- read.csv("LCnames.csv")
data <- data %>% left_join(LCnames, by = "landcover")

ggplot(data, aes(fill=LC_name, y=sum, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_ipsum()

ggplot(data, aes(fill=LC_name, y=sum, x=year)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_ipsum()

ggplot(data, aes(fill=year, y=sum, x= LC_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum()

ggplot(data %>% filter(LC_name %in% c("agriculture", "barren", "developed", "snow/ice", "water", "wetland")), aes(fill=year, y=sum, x= LC_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_ipsum()
