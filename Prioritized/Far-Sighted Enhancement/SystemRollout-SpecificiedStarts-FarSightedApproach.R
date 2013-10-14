#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

rm(list=ls())
#libraries that all may or may not be used in script
library(plyr)
library(ggplot2)
library(sp)
library(rgeos)
library(geosphere)
library(maptools)
library(stringr)
library(PBSmapping)

require(gdata)


# ##Jonathan's directory
setwd("~/Dropbox/Liberia_Geospatial_Analysis/Liberia_Modeling/2013-05-06/NPOutputs/3X-200Max-N360")

# #specify directory that shapefiles sits within 
#folder <- "/Users/SharedSolar/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/R scripts/JC-working/230/"



#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 


prioritized.grid.farsighted <- function(prioritized.grid) 
{
#   Apply a Far Sighted Global Ordering to Dataset for rollout
#   0. Create a New dynamic variable called branch.root that will change with each loop iteration
  prioritized.grid$root.branch <- prioritized.grid$root
  
#   1. Determine All candidate nodes again.  
  #1.0 address NA roots of subnetworks - HACKED
  subs <- subset(prioritized.grid, is.na(prioritized.grid$root))
  prioritized.grid$root.branch[which(is.na(prioritized.grid$root))] <- (str_sub(subs$branch,1,3))
  prioritized.grid$root[which(is.na(prioritized.grid$root))] <- (str_sub(subs$branch,1,3))
  #
  candidate.nodes <- prioritized.grid[which(prioritized.grid$branch == prioritized.grid$root),]
  non.candidate.nodes <- prioritized.grid[which(prioritized.grid$branch != prioritized.grid$root),]         

  #   2.Build a list of candidate.nodes$branch.root

  #   3.Usse ddply to summate all 'dist' and 'kWh' figures 
  
  branch.roots <- ddply(rbind.data.frame(candidate.nodes, non.candidate.nodes), .(root.branch), summarize, 
                     Total.MV.dist = sum(dist, na.rm=T), 
                     Total.Demand.kWh = sum(Demand...Projected.nodal.demand.per.year))
  
#3.1  MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year
  branch.roots <- mutate(branch.roots, Cummulative.Dist.per.kWh = Total.MV.dist/Total.Demand.kWh) 
  
#   4. Assign summations of MV lenght & kWh served to the respective candidate node list
 
  candidate.nodes <- merge(candidate.nodes,
                            subset(branch.roots, select = c(root.branch, Cummulative.Dist.per.kWh)),
                           by = "root.branch"
                           )
  #Function to rebuild ranked settlement dataframe network based on far sighted approach  
  
  branch_identify <- function(candidate_df, non_candidate_df)
  {
    #reset counters to 0 and clear any past rank.settlements dataframe 
    sequence <- as.integer(1)
    ranked.settlements <- data.frame(NULL)
    
    #     non_candidate_df <- non.candidate.nodes
    #     candidate_df <- candidate.nodes
    
    while (dim(candidate_df)[1] > 0) 
    {
      
      #   5. Select Candidate node such that Max [Sum kWh]/[Sum dist]

      #Selection criteria is based on 'far-sighted' sum AKA most effective cummulative summations by branch 
      candidate <- subset(candidate_df, Cummulative.Dist.per.kWh == min(Cummulative.Dist.per.kWh))[1,]
      candidate$far.sighted.sequence <- sequence
      sequence <- sequence + 1 
      ## 5.1 remove candidate observation from candidate.nodes
      candidate_df <- subset(candidate_df, !(Settlement.id %in% candidate$Settlement.id))
      
      #   6. If non.candidate$branch.roots == candidate$root then non.canddidate$branch.root == candidate$branch
      #   7. Re-Build Candidate and non-candidate lists
      #   8. Repeat 
      
      ## 5.2 place candidate in next order of ranked.settlements and assign incremental value for "sequence"
      ranked.settlements <- rbind(ranked.settlements, candidate)
      
      ## re-build candidate.nodes by pulling in any settlements that share "group" with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
      #identify all new lines associated with candidate settlement 
      ##new.root.branches <- unique(non_candidate_df$root.branch[which((non_candidate_df$root.branch == candidate$root.branch))])
      old.root.branch <- candidate$root.branch
      
      #determine non_candidate node family if any affected by the network addition 
      candidate.family <- subset(non_candidate_df, non_candidate_df$root.branch == old.root.branch)
      candidate.family$root.free.branch <- str_sub(candidate.family$branch, str_length(old.root.branch)+2)
      #identify new segments enabled by adding the previous candidate to the network
      new.segments <- candidate.family[which(
        is.na(str_locate(candidate.family$root.free.branch,"-"))[,2]),]
      #re-define the root-branch for these new segments as their own branch ID
      #so all subsequent nodes can be identified by them
      new.segments$root.branch <- new.segments$branch
      
      #reassociate all downstream nodes' root.branch id with new candidate nodes' branch id
      #I think this needs to occur in a loop in the cases where more than 1 root.branches are invovled
      #df$root.branch <- new.candidates$branch
      
      if (dim(new.segments)[1]>0) 
      {
        #recategorize all non.candidate nodes root.branches to correspond to new upstream node(s)
        for (i in 1:length(new.segments$root.branch)) 
          {
          #Sort non-candidates that should have root.branches updated
          length.root <- str_length(new.segments$root.branch[i])
          new.root.branch <- new.segments$root.branch[i]
          #(str_sub(non_candidate_df$branch, end=length.root) == new.root.branch)
          non_candidate_df$root.branch[which(str_sub(non_candidate_df$branch, end=length.root) 
                                             == new.root.branch)] <- new.segments$branch[i]
        }
        #Make new candidates compatible with existign candidate_df
        dummy.column.location <- grep('root.free.branch', colnames(new.segments))
        new.candidates <- subset(new.segments, select = -dummy.column.location)    
        new.candidates$Cummulative.Dist.per.kWh <- NA 
        #Add new candidates to candidate_df
        candidate_df <- rbind(candidate_df, new.candidates)
        #remove non_candidate_df that are now candidates 
        non_candidate_df <- subset(non_candidate_df, !(Settlement.id %in% new.candidates$Settlement.id))
        
        #Re-Calclate down-stream sums of MV length and kWh consumed            
        branch.roots <- ddply(rbind.fill(candidate_df, non_candidate_df), .(root.branch), summarize, 
                              Total.MV.dist = sum(dist, na.rm=T), 
                              Total.Demand.kWh = sum(Demand...Projected.nodal.demand.per.year))
        
        #3.1  MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year
        branch.roots <- mutate(branch.roots, Cummulative.Dist.per.kWh = Total.MV.dist/Total.Demand.kWh) 
                
        #remove previous cummulative sumes
        candidate_df$Cummulative.Dist.per.kWh <- NULL
        
        # Assign new summations to the respective candidate node list
        candidate_df <- merge(candidate_df,
                                 subset(branch.roots, select = c(root.branch, Cummulative.Dist.per.kWh)),
                                 by = "root.branch")
                
     
        
      }
      
    }
    
    
    output <- vector("list",2)
    output[[1]] <- ranked.settlements
    output[[2]] <- non_candidate_df
    return(output)
  }
  
  output<- branch_identify(candidate.nodes, non.candidate.nodes)
  ranked.settlements <- output[[1]]
  
  return(ranked.settlements)
}

## 9.0 Test function and Output csv with "rankings"
test <- prioritized.grid(tes)
write.csv(test, "GridNodesRanked.csv", row.names=F)

new.local <- merge(test, local, all.y=T)
write.csv(new.local, "metrics-local-AllGridNodesRanked.csv", row.names=F)
