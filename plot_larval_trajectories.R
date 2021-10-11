## This code is for generating and plotting larval trajectories from release locations (in Zone 3) and daily positions (up to 60 days)
## Larval dispersal data from biophysical model originated from Xue et al. (2008)
## By Amanda Xuereb, edited October 2021

library(data.table)
library(tidyverse)
library(readr)
library(rgdal)
library(rgeos)
library(maptools)
library(plyr)
library(dplyr)
library(sp)
library(tidyr)


year = 2002  # change for 2002, 2003, 2004

##  Create 'settle' location files -- each 690 rows is a settlement location after x days (1-60 days). This will create 60 settle loc files (1 file per day). Run for each year (2002, 2003, 2004)

all.settle.data <- fread(paste0("/path/to/folder/",year,"_0601_daily_positions_after_release.txt"), header = T, stringsAsFactors = F)  

nrow(all.settle.data)  # should equal 690*60


all.settle.data$Day <- rep(seq(1,60,1), each = 690)
data.split <- split(all.settle.data, all.settle.data$Day)
head(data.split)
length(data.split) # 60 days

names(data.split) <- paste(year, "0601", sprintf("%02s",names(data.split)), sep = "_") 
names(data.split)

sapply(names(data.split), function (x) write.table(data.split[[x]], file = paste("/path/to/folder/settle_locs/lobster_traj_", sprintf("%02s",x), ".txt", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE))  

## Next, generate the trajectories for each release date and for each site (2 release sites). 
rm(list = ls())

year <- 2002  # change for 2002, 2003, 2004

## RELEASE PARTICLE LOCATIONS

releases <- read.table(paste0("/path/to/file/release_",year,"_0601.txt"), header = T, stringsAsFactors = F)
head(releases)

xy.rel <- releases[,c(2,3)]
spdf.rel <- SpatialPointsDataFrame(coords = xy.rel, data = releases, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


###  DAILY PARTICLE POSITIONS 

# set working directory to the location where the settlement location files (settle_locs) for each year that were created above are stored. There should be 1 file per day (x 60 days) in this folder.
setwd("/path/to/settle_locs/")
length(dir())

settle <- list()
xy.set <- list()
spdf.set <- list()

for (i in 1: length(dir())) {
  settle[[i]] <- fread(dir()[i], header = T, stringsAsFactors = F)
  xy.set[[i]] <- settle[[i]][,c(2,3)]
  spdf.set[[i]] <- SpatialPointsDataFrame(coords = xy.set[[i]], data = settle[[i]], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
}


## using ggmap 

library(readr)
library(mapproj)
library(raster)
library(gpclib)
library(ggplot2) # v. 2.2.1
library(shapefiles)
library(ggmap) # v. 2.7
library(sp)
library(RgoogleMaps)
library(dplyr)

# GENERATE MAP FOR GULF OF MAINE
my_location <- c(-72,38,-61, 46)
myMap <- get_stamenmap(my_location, zoom = 6, maptype="toner-background")  

## To change the black background to another colour:
attr_myMap <- attr(myMap, "bb") # save attributes from original
myMap[myMap == "#000000"] <- 'gray90' 
class(myMap) <- c("ggmap", "raster")
attr(myMap, "bb") <- attr_myMap

ggmap(myMap)

### use spdf.set to plot all daily larval positions 
spdf.set.dfs <- list()
for (j in 1:length(spdf.set)) {
  spdf.set.dfs[[j]] <- spdf.set[[j]]@data
}

spdf.set.all <- bind_rows(spdf.set.dfs, .id = "column_label")

spdf.set.filter <- filter(spdf.set.all, ind_ID %in% c(15830:16289))    # this was used just to plot a smaller number of points on the map for easier visualization (this plots 100 individual larvae at 60 daily positions). The same individual IDs are plotted in each year


sites.map <- ggmap(myMap) +
  geom_blank() +
  geom_point(aes(x = long0, y = lat0, col = Day), data = spdf.set.filter, shape = 16, size = 1, alpha = 0.7) +
  scale_color_gradient(low = "#54278f", high = "#9e9ac8") +
   labs(x = "Longitude", y = "Latitude") + 
  scale_x_continuous(name = "Longitude", breaks = seq(-72, -62, 1), limits = c(-72,-61.5), expand = c(0,0), sec.axis = dup_axis()) +
  scale_y_continuous(name = "Latitude", breaks = seq(39, 46, 1), limits = c(38.9,46), expand = c(0,0), sec.axis = dup_axis()) +
  theme_classic() +
  theme(axis.text.x.bottom = element_text(size=12, color = "black", margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")), 
        axis.title.x.bottom = element_text(size = 14), 
        axis.text.x.top = element_blank(),
        axis.title.x.top = element_blank(),
        axis.ticks.length.x.bottom = unit(-0.15, "cm"),
        axis.ticks.length.x.top = unit(-0.15, "cm"),
        axis.title.y.left = element_text(size = 14), 
        axis.title.y.right = element_blank(), 
        axis.text.y.left = element_text(size = 12, color = "black", margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.text.y.right = element_blank(),
        axis.ticks.length.y.left = unit(-0.15,"cm"),
        axis.ticks.length.y.right = unit(-0.15,"cm"),
        
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=0.6))


sites.map

ggsave(filename = "/path/to/output/folder/",year,"_0601_100larvae_60days.png", plot = sites.map, device = "png", width = 7, height = 5, units = "in")

