library(plyr)
library(ggplot2)
library(sp)
library(rgeos)
library(geosphere)
library(maptools)
library(stringr)
library(PBSmapping)

require(gdata)


###load snapshot
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/0X-200Max-snapshot-N361")
local.snapshot <- read.csv("metrics-local.csv", skip=1)
local.snapshot$Settlement.id <- rownames(local)
proposed.snapshot <- readShapeLines("networks-proposed.shp")

###load 1X
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/1X-200Max-N356")
local.1X <- read.csv("metrics-local.csv", skip=1)
local.1X$Settlement.id <- rownames(local)
proposed.1X <- readShapeLines("networks-proposed.shp")

###load 2X
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/2X-200Max-N358")
local.2X <- read.csv("metrics-local.csv", skip=1)
local.2X$Settlement.id <- rownames(local)
proposed.2X <- readShapeLines("networks-proposed.shp")

###load 3X
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/3X-200Max-N360")
local.3X <- read.csv("metrics-local.csv", skip=1)
local.3X$Settlement.id <- rownames(local)
proposed.3X <- readShapeLines("networks-proposed.shp")

#same projection for all scenarios
proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)


####Priority Function#########
prioritized.grid <- function(local_df, shape.file, proj_var = proj4) 
{
  dist_fun <- function(points_1, points_2, projection_type = proj4)
  {
    if(grepl("utm", projection_type[1,1]))
    {
      points_1 <- as.matrix(points_1)
      if (dim(points_1)[1] != dim(points_2)[1])
      {
        points_1 <- matrix(rep(points_1,dim(points_2)[1]),ncol=2,byrow=T)      
      }
      points_2 <- as.matrix(points_2)
      dist <- sqrt(rowSums((points_1 - points_2)^2))  
    }else{
      dist <- distCosine(points_1,points_2,r=6371010) 
      #r=6371010  --> radius of Earth Network Planner uses  
    } 
    return(dist)
  }
  
  #rename X & Y to long and lat so correspond exactly with proposed grid shape file
  names(local_df)[names(local_df)=="X"] <- "long"
  names(local_df)[names(local_df)=="Y"] <- "lat"
  #subset and truncate local file for most relevant values
  local.grid <- subset(local_df, Metric...System=="grid")
  local.grid <- local.grid[,c("Name",
                              "Settlement.id",
                              "long",
                              "lat",
                              "Metric...System",
                              "Demographics...Projected.household.count",
                              "Demand..household....Target.household.count",
                              "Demand...Projected.nodal.demand.per.year",
                              "System..grid....Transformer.cost",
                              "Demographics...Population.count",
                              "Demographics...Projected.population.count")
                           ]
  
  ## 2.0 Fortify (or flatten) proposed in order to evaluate each vertex or point on shape file lines -> proposed.fortified
  #Fortify shape file to allow for analysis
  proposed.fortified <- fortify(shape.file)
  
  ## 3.0 Merge local.grid along with proposed.fortified vertexe nodes 
  ## ensuring ALL non-matchimg proposed.fortified nodes are kept in the merge -> merged
  merged <- merge(local.grid, proposed.fortified, all.y = T)
  merged$fake.node <- F
  merged[which(is.na(merged$Settlement.id)), "fake.node"] <- T
  
  ## 4.0 Each "fake.node" will define the start of a unique branch and should be given a "branch.id" from 1 to n
  branch <- (1:sum(as.numeric(merged$fake.node == T)))
  merged$branch <- NA 
  merged$root <- NA  ##keep an account of which root node branches originate from 
  merged$branch[which(merged$fake.node == T)] <- branch
  merged$root[which(merged$fake.node == T)] <- branch
  
  ###** convert to lat/long here if needed determine distance between all nodes
  #   if(grepl("utm", proj4[1,1])) 
  #   {
  #     ## zone <-  regmatches(+zone="XX") // identify which zone projection is in from 1-60 based on proj4
  #     ##best to extract zone from proj4 string if possible
  #     attr(merged, "zone") <- 29
  #     attr(merged, "projection") <- "UTM"
  #     names(merged)[names(merged)=="long"] <- "X"
  #     names(merged)[names(merged)=="lat"] <- "Y"
  #     merged <- convUL(merged, km=F, southern=F)
  #     names(merged)[names(merged)=="X"] <- "long"
  #     names(merged)[names(merged)=="Y"] <- "lat"
  #   }
  # 
  
  ## 5.0 Find all nodes that match with segments, aka in same "group",  of "fake.nodes"
  ## and label them as "start.nodes" preserving the "branch.id" and "distance" values
  ghost.nodes <- subset(merged, (fake.node == TRUE))
  ghost.nodes <- arrange(ghost.nodes, id)
  
  ## 6.0 Remove/sub-set all settlements that are "start.nodes" from merged  to new dataframe -> candidate.nodes
  candidate.nodes <- subset(merged, 
                            group %in% ghost.nodes$group 
                            & fake.node==FALSE)
  
  # use arrange function instead ddply
  candidate.nodes <- arrange(candidate.nodes , id)
  
  #preserve unique branch & root codes originating from existing network 
  candidate.nodes$branch <- ghost.nodes$branch
  candidate.nodes$root <- ghost.nodes$branch
  
  #remove candidate nodes from non.candidate nodes dataframe
  non.candidate.nodes <- subset(merged, !(id %in% ghost.nodes$id))
  
  # 7.0 Build new dataframe for ranked settlements -> ranked.settlements
  ## 7.1 determine distance between all nodes
  dist <-  dist_fun(candidate.nodes[,1:2],
                    ghost.nodes[,1:2],
                    proj_var)
  #it is important to use dummy variable Settlement.id here since Names is not always unique
  dist <- data.frame(dist, Settlement.id = candidate.nodes[,"Settlement.id"])
  
  #reassign distance to candidate.nodes dataframe attributing all length to destination or candidate nodes
  new.candidate.nodes <- merge(candidate.nodes, dist)
  
  #calculate the unitized MV line required per annual kWh delivered to connect each candidate node
  new.candidate.nodes <- mutate(new.candidate.nodes, 
                                MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year)
  
  branch_identify <- function(candidate_df, non_candidate_df)
  {
    ## 8.0 For 1:nobs merged 
    ## 8.1 select the observation from candidate.nodes that has the minimum grid length/HH connected <- best.candidate
    sequence <- as.integer(1)
    ranked.settlements <- data.frame(NULL)
    
    #     non_candidate_df <- non.candidate.nodes
    #     candidate_df <- new.candidate.nodes
    
    while (dim(candidate_df)[1] > 0) 
    {
      candidate <- subset(candidate_df, MV.line.per.kwh == min(MV.line.per.kwh))[1,]
      candidate <- mutate(candidate, 
                          sequence = sequence)
      sequence <- sequence + 1
      ## 8.2 remove candidate observation from candidate.nodes
      candidate_df <- subset(candidate_df, !(Settlement.id %in% candidate$Settlement.id))
      ## place candidate in next order of ranked.settlements and assign incremental value for "sequence"
      ranked.settlements <- rbind(ranked.settlements, candidate)
      
      ## 8.3 re-build candidate.nodes by pulling in any settlements that share "group" with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
      #identify all new lines associated with candidate settlement 
      new.segments <- non_candidate_df$id[which((non_candidate_df$Settlement.id == candidate$Settlement.id))] 
      if (length(new.segments)>0) 
      {
        #creates 12 variable dataframe such that lines connect to candidate node and settlements are not the candidate node themself
        new.candidate.nodes2 <- as.data.frame(subset(non_candidate_df, (id %in% new.segments) &
                                                       (Settlement.id != candidate$Settlement.id)))
        #ensure unique branch labeling scheme is in place showing ancestory 
        v1 <- 1:length(new.segments)
        new.candidate.nodes2$branch <- str_c(candidate$branch,"-",v1)
        new.candidate.nodes2$root <- (candidate$root)
        
        #remove non_candidate_df that are now candidates 
        non_candidate_df <- subset(non_candidate_df, !(id %in% new.segments))  
        
        #calculate the distance to the new.candidate.nodes
        #^. use the new distance function 
        new.dist <-  dist_fun(candidate[,c("long", "lat")],
                              new.candidate.nodes2[,c("long", "lat")], proj_var) #new distance function by Zaiming!
        
        new.dist <- data.frame(dist = new.dist, Settlement.id = new.candidate.nodes2[,"Settlement.id"])
        
        #makes compatible 15 variable dataframe for candidate.nodes
        new.candidate.nodes2 <- merge(new.candidate.nodes2, new.dist)    
        ##so here is is an issue where 2 disimilar types are being combined to make a list and not a dataframe
        new.candidate.nodes2 <- mutate(new.candidate.nodes2, 
                                       MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year
        )                              
        candidate_df <- rbind(candidate_df, new.candidate.nodes2)
        ## 12.4 Return to step 12.1 until merged dataframe is completely sorted into ranked.settlement 
      }
      
    }
    
    
    output <- vector("list",2)
    output[[1]] <- ranked.settlements
    output[[2]] <- non_candidate_df
    return(output)
  }
  
  
  ###Starting point of the subnetwork ranking function 
  sub.network.candidate <- function(input_non_candidate_df)
  {
    #####identify the sub-networks
    non_candidate_df <- input_non_candidate_df
    
    ###here is something on top of the whole loop
    sub.network.groups <- vector("list")
    counter <- 1
    
    ##outer loop stars here
    while(length(unique(non_candidate_df$Settlement.id)) != 0)
    {
      candidate.groups<- unique(non_candidate_df$Settlement.id)
      
      #initialized with one node
      node.settlement.ids <- candidate.groups[1]
      exit <- F
      
      # Inner loop starts from here
      while(exit == F)
      {
        #pick all the segment id associated with that node
        connected.segments <- unique(non_candidate_df[which(non_candidate_df$Settlement.id %in% node.settlement.ids),"id"])
        if (length(connected.segments) == 0)
        {
          exit <- T    
        }
        #search all node Settlement.id associated with the segment
        connected.nodes <- unique(non_candidate_df[which(non_candidate_df$id %in% connected.segments),"Settlement.id"])
        #output the Settlement IDs 
        node.settlement.ids <- unique(c(node.settlement.ids, connected.nodes))
        #delete all the finded nodes from subnetwork
        non_candidate_df <- subset(non_candidate_df, !(id %in% connected.segments))
        #candidate.groups<- unique(non_candidate_df$Name)
      }
      sub.network.groups[[counter]] <- subset(input_non_candidate_df, Settlement.id %in% node.settlement.ids)
      counter <- counter + 1
    }
    
    #define the function returns the max kWh demand served per node 
    max.demand <- function(df, demo_col_name="Demand...Projected.nodal.demand.per.year")
    {
      return(df[which(df[,demo_col_name] == max(df[,demo_col_name])),][1,])
    }
    candidates.nodes <- ldply(sub.network.groups, max.demand)
    candidates.nodes <- arrange(candidates.nodes, desc(Demand...Projected.nodal.demand.per.year))
    branch <- paste("S",(1:length(sub.network.groups)), sep="-")
    candidates.nodes$branch <- branch
    
    candidates.nodes <- subset(candidates.nodes, select=c("Settlement.id", "id", "branch"))
    
    get.branch <- function(df)
    {
      df$branch <- NULL
      df <- merge(df, candidates.nodes, by = c("Settlement.id", "id"), all.x = T)
      new.candidate.nodes <- subset(df, !is.na(branch))
      new.candidate.nodes <- mutate(new.candidate.nodes, 
                                    dist = 0,
                                    MV.line.per.kwh = 0)
      #         non.candidate.nodes <- subset(df, is.na(branch))
      non.candidate.nodes <- df
      output <- branch_identify(new.candidate.nodes, non.candidate.nodes)
      return(output[[1]])
    }
    ranked.sub.networks <- ldply(sub.network.groups, get.branch)
    
    
    
    return(ranked.sub.networks)
  }
  
  output<- branch_identify(new.candidate.nodes, non.candidate.nodes)
  ranked.settlements <- output[[1]]
  ranked.subnetworks <- sub.network.candidate(output[[2]])
  combined.networks <- rbind.fill(ranked.settlements, ranked.subnetworks)
  
  return(combined.networks)
}

local.snapshot.sorted <- prioritized.grid(local.snapshot, proposed.snapshot)
local.1X.sorted <- prioritized.grid(local.1X, proposed.1X)
local.2X.sorted <- prioritized.grid(local.2X, proposed.2X)
local.3X.sorted <- prioritized.grid(local.3X, proposed.3X)


##Output csv with 4 scenarios sorted side by side grid nodes


local.snapshot.short <- local.snapshot.sorted[,c("Settlement.id",
                                     "sequence")]
colnames(local.snapshot.short)[2] <- "sequence.snapshot"

local.1X.short <- local.1X.sorted[,c("Settlement.id",
                                "sequence")]
colnames(local.1X.short)[2] <- "sequence.1X"

local.2X.short <- local.2X.sorted[,c("Settlement.id",
                                     "sequence")]
colnames(local.2X.short)[2] <- "sequence.2X"


local.3X.short <- local.3X.sorted[,c("Settlement.id",
                                     "sequence")]
colnames(local.3X.short)[2] <- "sequence.3X"
colnames(local.3X.sorted)[21] <- "sequence.3X"


##Merge shortened dataset

test <- merge(local.snapshot.short, local.1X.short, all = TRUE)
test <- merge(test, local.2X.short, all = TRUE)
test <- merge(test, local.3X.sorted, all = TRUE)

##Output csv 
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs")
write.csv(test, "staged-connections-for-200max-series.csv", row.names=F)
