

## jonathans dropbox directory 
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/3X-200Max-N360")


##1.0 - Import the master merged dataset of metrics local with bins and MV length assigned
master <- read.csv("MasterMerged_20130628.csv")

#2.0 - Group nodes to master system by root
#develop a system column
master$Geographic.System <- NA

#Mano Substation 
master$Geographic.System[master$root %in% c(3,1)]<- "ManoSubstation"








##3.0 Output csv now with systems defined 
write.csv(master, "MasterMerged_20130701_DefinedSystems.csv", row.names=F)

                            