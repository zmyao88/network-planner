##Geo-spatial analysis for mv grids
require(maptools)
require(rgdal)
require(ggplot2)
require(geosphere)
require(plyr)
require(RCurl)
require(XML)
setwd("C:/Users/zmyao/Dropbox/Network Planning/230/")
# 
grid <- readShapeLines("networks-proposed.shp")
local <- read.csv("./metrics-local.csv", stringsAsFactors=F, skip=1)
local$settlement_id <- rownames(local)
proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)

##############$$$$$$$$$$$$$$$$$$$$$$$
# temp <- tempfile()
# download.file("http://networkplanner.modilabs.org/scenarios/1988.zip",temp)
# data <- read.csv(unz(temp, "metrics-local.csv"), stringsAsFactors=F, skip=1)
# unlink(temp)

download_scenario <- function(scenario.number)
{
    scenario.number <- 1988
    temp <- tempfile()
    get_zip_url<- function(SN)
    {
        zip_url <- paste('http://networkplanner.modilabs.org/scenarios/', SN, '.zip', sep="")
        return (zip_url)
    }
    
    download.file(get_zip_url(secnario.number),temp)
    
    grid <- readShapeLines(unz(temp, "networks-proposed.shp"))
    local <- read.csv(unz(temp, "metrics-local.csv"), stringsAsFactors=F, skip=1)
    local <- read.csv(unz(temp, "metrics-local.csv"), stringsAsFactors=F, skip=1)
    proj4 <- read.csv(unz(temp,"metrics-local.csv"), nrows=1, header = FALSE)
    global <- read.csv(unz(temp, "metrics-global.csv"), stringsAsFactors=F, skip=1)
    
    
    unlink(temp)
    
    output_list <- vector("list",4)
    output_list[[1]] <- local
    output_list[[2]] <- global
    output_list[[3]] <- grid
    output_list[[4]] <- proj4
    return(output_list)
}



zip <- getURI("http://networkplanner.modilabs.org/scenarios/1988.zip")
test <- getURI("http://networkplanner.modilabs.org/scenarios", userpwd=paste("cnatali", "cnatali", sep=":"), httpauth = 1L)
test_parsed <- htmlTreeParse(test)
test_parsed[3]
# getNodeSet(test_parsed, "//*/div[@class='scenario odd']")
# 
# //*[@id=]
# 
# //*[@id="scenario2067"]/td[6]/a[2]
# 
# http://networkplanner.modilabs.org/scenarios/91.zip








#################$$$$$$$$$$$$$$$$$$$$$
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

# Introducint the Boolena flag variable proj4 to detect if there is string "units=m" in the 1st line in Metrics-local
if(grepl("units=m", proj4[1,1])) 
{
    dist <- sqrt(rowSums((data_split[[1]][,1:2] - data_split[[2]][,1:2])^2))
}else{
    dist <- distCosine(data_split[[1]][,1:2],data_split[[2]][,1:2],r=6371010) 
} 
dist <- data.frame(dist , id = data_split[[1]][,"id"])

#  Double the length of gird lines that has only one connection wiht settlement
dist[dist$id %in% single_connections,"dist"] <- dist[dist$id %in% single_connections,"dist"] * 2

# Merge grid line length with dots & line data
new_prop_grid <- merge(new_prop_grid, dist)
new_prop_grid <- new_prop_grid[order(new_prop_grid$settlement_id), ]

grid_length_attr <- ddply(new_prop_grid, .(settlement_id), summarize, 
                          half_length =  sum(dist)/2)

local_dist <- merge(local, grid_length_attr, by="settlement_id", all.x=T)
local_dist[, "settlement_id"] <- NULL

write.csv(local_dist, "local_metrics_local_w_Mv_grid_length.csv", row.names=F)



######## ViZ
library(ggmap)
library(mapproj)

local$Metric...System <- factor(local$Metric...System, 
                        levels=c("unelectrified", "off-grid", "mini-grid", "grid"))
# theme_set(theme_bw(16))
loc <- c(mean(settlement$long), mean(settlement$lat))
# map <- get_map(location= "maumere", zoom = 12)
map <- get_map(location= loc, zoom = 8)
PlotMap <- ggmap(map, legend = "topleft")
PlotMap + geom_point(aes(x = X, y = Y, size = Metric...System,colour = Metric...System),data = local)  + 
    geom_line(data = grid_data, aes(x = long, y = lat, group = id, alpha=0.4))
    
# geom_polygon(data = grid_data, aes(x = long, y = lat, group = id))
# PlotMap + geom_point(aes(x = long, y = lat, alpha = 0.3, color="red"), data = settlement)