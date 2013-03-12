##Geo-spatial analysis for mv grids
require(maptools)
require(rgdal)
require(ggplot2)
require(geosphere)
require(plyr)

setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

grid <- readShapeLines("networks-proposed.shp")
local <- read.csv("./metrics-local.csv", stringsAsFactors=F, skip=1)

##Checking the data
class(grid)
names(grid)
str(grid)
grid@lines[[1]]


### Simple data manipulation
#change shape data into flat file 
grid_data <- fortify(grid)
settlement <- local[,c("X", "Y", "Metric...System")]
settlement$settlement_id <- rownames(settlement)
names(settlement)[1:2] <- c("long", "lat")

# Merge dots with lines
new_prop_grid <- merge(grid_data,settlement)
new_prop_grid_2 <- merge(grid_data,settlement, all.x=T)

test <- settlement[settlement$Metric...System == "grid", ]
sum(test %in% new_prop_grid_2$point_id)

test2 <- merge(test,local, by="row.names")
test2$settlement_id
row.names(test)


# Calculate distance for each grid lines using vincent ellipsoid method
# Assuming the Big Circle is a ellipses rather than a sphere
data_split <- dlply(grid_data, .(order))
#p1 <- data_split[[1]]
#p2 <- data_split[[2]]
#dist <- distVincentyEllipsoid(data_split[[1]][,1:2],data_split[[2]][,1:2])
dist <- distCosine(data_split[[1]][,1:2],data_split[[2]][,1:2],r=6371010)
dist <- data.frame(dist , id = data_split[[1]][,"id"])

# Merge grid line length with dots & line data
new_prop_grid <- merge(new_prop_grid, dist)
new_prop_grid <- new_prop_grid[order(new_prop_grid$settlement_id), ]

grid_length_attr <- ddply(new_prop_grid, .(settlement_id), summarize, 
                          half_length =  sum(dist)/2)
