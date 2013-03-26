#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the proposed.grid network.  

rm(list=ls())
#libraries that all may or may not be used in script
library(plyr)
library(ggplot2)
library(rgeos)
library(sp)
library(geosphere)
library(maptools)
library(stringr)



#library(rgdal)
#not sure if i use these too... 
#library(foreign)
#library(grid)
#library(lattice)


# ##Jonathan's directory
# setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/R scripts/JC-working/230")
# #specify directory that shapefiles sits within 
# folder <- "/Users/SharedSolar/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/R scripts/JC-working/230/"

##Edwin's directory
#setwd("C:/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2013/256-HHDem240/")  
#mainDir <- "C:/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/R scripts/JC-working"

##Zaiming's directory 
# folder <- "C:/Users/zmyao/Dropbox/Network Planning/230"
setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

##1.0 - Import metrics.local for only grid-proposed nodes -> local.grid
#load metrics.local to associated settlement points with proposed grid data
local <- read.csv("metrics-local.csv", skip=1)
proposed <- readShapeLines("networks-proposed.shp")

prioritized.grid <- function(local_df, shape.file)
{
    #rename X & Y to long and lat so correspond exactly with proposed grid shape file
    names(local_df)[names(local_df)=="X"] <- "long"
    names(local_df)[names(local_df)=="Y"] <- "lat"
    #subset and truncate local file for most relevant values
    local.grid <- subset(local_df, Metric...System=="grid")
    local.grid <- local.grid[,c("Name",
                                "long",
                                "lat",
                                "Metric...System",
                                "Demographics...Projected.household.count")
                             ]
    
    ##2.0 import proposed grid .shp file -> proposed
    # proposed <- readOGR(folder, "networks-proposed")
    
    
    ## 3.0 Fortify (or flatten) proposed in order to evaluate each vertex or point on shape file lines -> proposed.fortified
    #Fortify shape file to allow for analysis
    proposed.fortified <- fortify(shape.file)
    
    ## 4.0 Merge local.grid along with proposed.fortified vertexe nodes 
    ## ensuring ALL non-matchimg proposed.fortified nodes are kept in the merge -> merged
    merged <- merge(local.grid, proposed.fortified, all.y = T)
    merged$fake.node <- F
    merged[which(is.na(merged$Name)), "fake.node"] <- T
    
    
    ## 8.0 Each "fake.node" will define the start of a unique branch and should be given a "branch.id" from 1 to n
    branch <- (1:sum(as.numeric(merged$fake.node == T)))
    merged$branch <- NA 
    merged$branch[which(merged$fake.node == T)] <- branch
    
    ## 9.0 Find all nodes that match with segments, aka in same "group",  of "fake.nodes"
    ## and label them as "start.nodes" preserving the "branch.id" and "distance" values
    ghost.nodes <- subset(merged, (fake.node == TRUE))
    ghost.nodes <- arrange(ghost.nodes, id)
    
    ### 9.1 determine distance between all nodes
    ##Does not work here as the order between nodes are not consistent so moved to step 11.1 & 12.3 
    
    # ^.
    # ^. Not quite sure what are you trying to do in this step, will revise this part later
    # ^.
    
    ## 10.0 Remove/sub-set all settlements that are "start.nodes" from merged  to new dataframe -> candidate.nodes
    candidate.nodes <- subset(merged, 
                              group %in% ghost.nodes$group 
                              & fake.node==FALSE)
    # use arrange function instead ddply
    candidate.nodes <- arrange(candidate.nodes , id)
    
    #preserve unique branch code originating from existing network 
    candidate.nodes$branch <- ghost.nodes$branch
    #remove candidate nodes from non.candidate nodes dataframe
    
    non.candidate.nodes <- subset(merged, !(id %in% ghost.nodes$id))
    
    ## 11.0 Build new dataframe for ranked settlements -> ranked.settlements
    ## 11.1 determine distance between all nodes
    # data_split <- dlply(non.candidate.nodes, .(order))
    dist <-  distVincentySphere(candidate.nodes[,1:2],
                                ghost.nodes[,1:2],
                                r=6371010) #radius of Earth Network Planner uses
    dist <- data.frame(dist, Name = candidate.nodes[,"Name"])
    
    #reassign distance to candidate.nodes dataframe attributing all length to destination or candidate nodes
    new.candidate.nodes <- merge(candidate.nodes, dist)
    #calculate the unitized MV line required per HH to connect each candidate
    new.candidate.nodes <- mutate(new.candidate.nodes, 
                                  MV.line = dist/Demographics...Projected.household.count)
    
    
    
    
    
    
    branch_identify <- function(candidate_df, non_candidate_df)
    {
        ## 12.0 For 1:nobs merged 
        ## 12.1 select the observation from candidate.nodes that has the minimum grid length/HH connected <- best.candidate
        sequence <- as.integer(1)
        ##settlements <- length(candidate.nodes)+length(unique(non_candidate_df$Name))
        ranked.settlements <- data.frame(NULL)
        #groups <- as.vector(NULL)
        #segments <- as.vector(NULL)
        
        while (dim(candidate_df)[1] > 0) {
            candidate <- subset(candidate_df, MV.line == min(MV.line))[1,]
            candidate <- mutate(candidate, 
                                sequence = sequence)
            sequence <- sequence + 1
            ## 12.2 remove candidate observation from candidate.nodes
            candidate_df <- subset(candidate_df, !(Name %in% candidate$Name))
            ## place candidate in next order of ranked.settlements and assign incremental value for "sequence"
            ranked.settlements <- rbind(ranked.settlements, candidate)
            
            ## 12.3 re-build candidate.nodes by pulling in any settlements that share "group" with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
            #identify all new lines associated with candidate settlement 
            new.segments <- non_candidate_df$id[which((non_candidate_df$Name == candidate$Name))] #& (non_candidate_df$id != candidate$id))]
            if (length(new.segments)>0) 
            {
                #segments <- c(segments, new.segments)
                #creates 12 variable dataframe such that lines connect to candidate node and settlements are not the candidate node themself
                new.candidate.nodes2 <- as.data.frame(subset(non_candidate_df, (id %in% new.segments) &
                                                                 (Name != candidate$Name)))
                #ensure unique branch labeling scheme is in place showing ancestory 
                v1 <- 1:length(new.segments)
                new.candidate.nodes2$branch <- str_c(candidate$branch,"-",v1)
                #remove non_candidate_df that are now candidates 
                #         non_candidate_df <- subset(non_candidate_df, !(Name %in% candidate$Name) & !(id %in% segments))  
                #         non_candidate_df <- subset(non_candidate_df, !(Name %in% candidate$Name) & !(id %in% new.segments))  
                
                #^. only needed to compare with nodes in new.seg because candidate is in new.segments & new.candidate.node2 is alos in new.seg 
                non_candidate_df <- subset(non_candidate_df, !(id %in% new.segments))  
                #calculate the distance to the new.candidate.nodes                                
                new.dist <-  distVincentySphere(candidate[,c("long", "lat")],
                                                new.candidate.nodes2[,c("long", "lat")],
                                                r=6371010) #radius of Earth Network Planner uses
                new.dist <- data.frame(dist = new.dist, Name = new.candidate.nodes2[,"Name"])
                #makes compatible 14 variable dataframe for candidate.nodes
                new.candidate.nodes2 <- merge(new.candidate.nodes2, new.dist)    
                ##so here is is an issue where 2 disimilar types are being combined to make a list and not a dataframe
                new.candidate.nodes2 <- mutate(new.candidate.nodes2, 
                                               MV.line = dist/Demographics...Projected.household.count
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
    
    output<- branch_identify(new.candidate.nodes, non.candidate.nodes)
    ranked.settlements <- output[[1]]
    non_candidate_df <- output[[2]]
    return(output)
}




test <- prioritized.grid(local,proposed)

t1 <- test[[1]]
t2 <- test[[2]]

## 13.0 Output csv and shape file with "rankings"
write.csv(ranked.settlements, "Ranked-Settlement-Nodes-V2.csv", row.names=F)
write.csv(non.candidate.nodes, "Sub-Network-Nodes-Unsorted-Unranked.csv", row.names=F)





##### identify subnetwork clusters

