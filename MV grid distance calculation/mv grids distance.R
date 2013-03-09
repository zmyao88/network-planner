##Geo-spatial analysis for mv grids
require(maptools)
require(rgdal)
require(ggplot2)
require(geosphere)
require(plyr)

setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

grid <- readShapeLines("networks-proposed.shp")
local <- read.csv("./metrics-local.csv", stringsAsFactors=F, skip=1)
class(grid)
names(grid)
str(grid)
grid@lines[[1]]

# Simple data manipulation
grid_data <- fortify(grid)
points <- local[,c("X", "Y")]
points$point_id <- rownames(points)
names(points)[1:2] <- c("long", "lat")

# Merge dots with lines
new_prop_grid <- merge(grid_data,points)

# Calculate distance for each grid lines using vincent ellipsoid method
# Assuming the Big Circle is a ellipsos rather than a sphere
data_split <- dlply(grid_data, .(order))
#p1 <- data_split[[1]]
#p2 <- data_split[[2]]
dist <- distVincentyEllipsoid(data_split[[1]][,1:2],data_split[[2]][,1:2])
dist <- data.frame(dist , id = data_split[[1]][,"id"])

# Merge grid line length with dots & line data
new_prop_grid <- merge(new_prop_grid, dist)
new_prop_grid <- new_prop_grid[order(new_prop_grid$point_id), ]

grid_length_attr <- ddply(new_prop_grid, .(point_id), summarize, 
                          half_length =  sum(dist)/length(dist))
