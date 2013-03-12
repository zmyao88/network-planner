##Geo-spatial analysis for mv grids
require(maptools)
require(rgdal)
require(ggplot2)
require(geosphere)
require(plyr)

setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

grid <- readShapeLines("networks-proposed.shp")
local <- read.csv("./metrics-local.csv", stringsAsFactors=F, skip=1)
local$settlement_id <- rownames(local)


##Checking the data
class(grid)
names(grid)
str(grid)
grid@lines[[1]]


### Simple data manipulation
# change shape data into flat file 
# Only settlement categorized in "grid" is connected 
grid_data <- fortify(grid)
settlement <- local[,c("X", "Y", "Metric...System", "settlement_id")]
names(settlement)[1:2] <- c("long", "lat")
settlement <- subset(settlement, Metric...System == "grid")

# Merge dots with lines
new_prop_grid <- merge(grid_data,settlement)
new_prop_grid_2 <- merge(grid_data,settlement, all.x=T)
single_connections <- new_prop_grid_2[is.na(new_prop_grid_2$settlement_id),"id"]

# Calculate distance for each grid lines using Cosine distance with Great Circle method
# Assuming the Big Circle is a sphere rather than a ellipses
data_split <- dlply(grid_data, .(order))


# proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)
# if(grepl("units=m", proj4[1,1])) 
#     distance <- distVincentySphere(p1, p2)  
# else 
#     distance <- distVincentyEllipsoid(p1, p2) 





# p1 <- data_split[[1]]
# p2 <- data_split[[2]]
# dist <- distVincentyEllipsoid(data_split[[1]][,1:2],data_split[[2]][,1:2])
dist <- distCosine(data_split[[1]][,1:2],data_split[[2]][,1:2],r=6371010)
dist <- data.frame(dist , id = data_split[[1]][,"id"])

#^  Double the length of gird lines that has only one connection wiht settlement
tt <- dist
dist[dist$id %in% single_connections,"dist"] <- dist[dist$id %in% single_connections,"dist"] * 2

# Merge grid line length with dots & line data
new_prop_grid <- merge(new_prop_grid, dist)
new_prop_grid <- new_prop_grid[order(new_prop_grid$settlement_id), ]

grid_length_attr <- ddply(new_prop_grid, .(settlement_id), summarize, 
                          half_length =  sum(dist)/2)
