#^. TO USE THIS FILE, PLEASE UN-ZIP THE INPUT FILE AND CHANGE THE WORKING DIRECTORY
#^. TO CHANGE THE WORKING DIRECTORY, 
#^. CHANGE THE ADDRESS of the folder in line 12 with setwd("FULL PATH OF YOUR FOLDER") e.g "~/Documents/Modi Labs/"


require(gdata)
require(plyr)
require(stringr)
require(data.table)

#Setting working directory to C:/Users/zmyao/Dropbox/Network Planning/230"
setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

#Load local metircs data
local <- read.csv("metrics-local.csv", skip=1,stringsAsFactors=F)
#name_str <- names(local)

#identify the location of 1st column that makes sense
#local2 <-  local[,which(str_detect(name_str, "Finance"))[1]:dim(local)[2]]

local_agg <- ddply(local, .(Metric...System), summarise, 
              sum_of_Population = sum(Population, na.rm=T), 
              sum_of_Demographics...Projected.household.count = sum(Demographics...Projected.household.count, na.rm=T), 
              sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
              sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand, na.rm=T),
              sum_of_Demand..peak....Projected.peak.nodal.demand = sum(Demand..peak....Projected.peak.nodal.demand, na.rm=T),
              sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.panel.actual.capacity = sum(System..off.grid....Photovoltaic.panel.actual.capacity, na.rm=T),
              sum_of_System..off.grid....System.initial.cost = sum(System..off.grid....System.initial.cost,na.rm=T),
              sum_of_System..off.grid....Photovoltaic.panel.cost = sum(System..off.grid....Photovoltaic.panel.cost, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.battery.cost = sum(System..off.grid....Photovoltaic.battery.cost, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.balance.cost = sum(System..off.grid....Photovoltaic.balance.cost, na.rm=T),
              sum_of_System..off.grid....System.recurring.cost.per.year =sum(System..off.grid....System.recurring.cost.per.year, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.battery.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.battery.replacement.cost.per.year, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.panel.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.panel.replacement.cost.per.year, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.balance.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.balance.replacement.cost.per.year, na.rm=T),
              sum_of_System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year = sum(System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year, na.rm=T),
              sum_of_System..off.grid....System.nodal.discounted.cost = sum(System..off.grid....System.nodal.discounted.cost, na.rm=T),
              avg_of_System..off.grid....System.nodal.levelized.cost = mean(System..off.grid....System.nodal.levelized.cost, na.rm=T),
              sum_of_System..mini.grid....Diesel.generator.actual.system.capacity = sum(System..mini.grid....Diesel.generator.actual.system.capacity,na.rm=T),
              sum_of_System..mini.grid....System.initial.cost = sum(System..mini.grid....System.initial.cost, na.rm=T),
              sum_of_System..mini.grid....Diesel.generator.cost = sum(System..mini.grid....Diesel.generator.cost,na.rm=T),
              sum_of_System..mini.grid....Diesel.generator.installation.cost = sum(System..mini.grid....Diesel.generator.installation.cost, na.rm=T),
              sum_of_System..mini.grid....Low.voltage.line.equipment.cost = sum(System..mini.grid....Low.voltage.line.equipment.cost, na.rm=T),
              sum_of_Distribution...Low.voltage.line.initial.cost = sum(Distribution...Low.voltage.line.initial.cost, na.rm=T),
              sum_of_System..mini.grid....System.recurring.cost.per.year = sum(System..mini.grid....System.recurring.cost.per.year, na.rm=T),
              sum_of_System..mini.grid....Diesel.fuel.cost.per.year = sum(System..mini.grid....Diesel.fuel.cost.per.year, na.rm=T),
              sum_of_System..mini.grid....Diesel.generator.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Diesel.generator.operations.and.maintenance.cost.per.year, na.rm=T),
              sum_of_System..mini.grid....Diesel.generator.replacement.cost.per.year = sum(System..mini.grid....Diesel.generator.replacement.cost.per.year, na.rm=T),
              sum_of_Distribution...Low.voltage.line.replacement.cost.per.year = sum(Distribution...Low.voltage.line.replacement.cost.per.year, na.rm=T),
              sum_of_System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year, na.rm=T),
              sum_of_System..mini.grid....System.nodal.discounted.cost = sum(System..mini.grid....System.nodal.discounted.cost, na.rm=T),
              avg_of_System..mini.grid....System.nodal.levelized.cost = mean(System..mini.grid....System.nodal.levelized.cost, na.rm=T),
              sum_of_System..grid....Grid.transformer.actual.system.capacity = sum(System..grid....Grid.transformer.actual.system.capacity, na.rm=T),
              sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost, na.rm=T),
              sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost,na.rm=T),
              sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost, na.rm=T),
              sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost, na.rm=T),
              sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year, na.rm=T),
              sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year, na.rm=T),
              sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year, na.rm=T),
              sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year, na.rm=T),
              sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost, na.rm=T),
              avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost, na.rm=T)
              )

# Load global data 
global <- read.csv("metrics-global.csv", header=F)
grid_type <- paste(global[,1], global[,2], sep=" ")
global <- as.matrix(global[,3])
row.names(global) <- grid_type

#######   which(row.names(global) == "system (off-grid) system total discounted diesel cost")

################################
####generating output tables####
################################
# Off-Grid
indicator <- c("Proposed Total watts of SHS", "Proposed new grid connections", "Total Initial cost for system", 
                         "System discounted cost", "Total demand met by SHS", "Total levelized cost per kWH for Grid power")
units <- c("KW", "Households", "$", "$", "KWh", "$")
total <- vector(mode="double",6)
per_HH <- vector(mode="double",6)
data <- t(as.matrix(subset(x=local_agg,Metric...System == "off-grid")))

total[1] <- data[rownames(data) == "sum_of_System..off.grid....Photovoltaic.panel.actual.capacity"]
total[2] <- data[rownames(data) == "sum_of_Demographics...Projected.household.count"]
total[3] <- data[rownames(data) == "sum_of_System..off.grid....System.initial.cost"]   
total[4] <- data[rownames(data) == "sum_of_System..off.grid....System.nodal.discounted.cost"]   
total[5] <- data[rownames(data) == "sum_of_Demand...Projected.nodal.discounted.demand"]   
total[6] <- data[rownames(data) == "avg_of_System..off.grid....System.nodal.levelized.cost"]   

total <- as.numeric(total)
per_HH <- total/total[2]
per_HH[c(2,6)] <- NA
off.grid.table <- data.frame(indicator,units,total,per_HH)




# Mini-Grid
indicator <- c("Proposed Total Capacity of Mini-Grids", "Proposed new mini-grid connections", "Total Initial cost for system", 
               "System discounted cost", "Total demand met by SHS", "Total levelized cost per kWH for Grid power")
units <- c("KW", "Households", "$", "$", "KWh", "$")
total <- vector(mode="double",6)
per_HH <- vector(mode="double",6)
data <- t(as.matrix(subset(x=local_agg,Metric...System == "mini-grid")))

total[1] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....Diesel.generator.actual.system.capacity"])*2/1000
total[2] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count"])
total[3] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.initial.cost"])
total[4] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.nodal.discounted.cost"])
total[5] <- as.numeric(data[rownames(data) == "sum_of_Demand...Projected.nodal.discounted.demand"])
total[6] <- as.numeric(data[rownames(data) == "avg_of_System..mini.grid....System.nodal.levelized.cost"])

total <- as.numeric(total)
per_HH <- total/total[2]
per_HH[c(2,6)] <- NA
mini.grid.table <- data.frame(indicator,units,total,per_HH)


# Grid
indicator <- c("Existing MV line length", "Proposed MV line length", "Proposed new grid connections", 
               "Total Initial cost for grid network (MV+LV)", "Total initial cost for MV grid network",
               "Total initial cost for LV grid network", "System discounted recurring cost",
               "Total demand met by Grid", "Total levelized cost per kWH for Grid power")
units <- c("Km", "Km", "Households", "$", "$", "$", "$", "KWh", "$")
total <- vector(mode="double",9)
per_HH <- vector(mode="double",9)
data <- t(as.matrix(subset(x=local_agg,Metric...System == "grid")))

total[1] <- global[rownames(global) == "system (grid) system total existing network length"]/1000
total[2] <- global[rownames(global) == "system (grid) system total proposed network length"]/1000
total[3] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count"])
total[4] <- global[rownames(global) == "system (grid) system total external initial cost"] + global[rownames(global) == "system (grid) system total internal initial cost"]
total[5] <- global[rownames(global) == "system (grid) system total external initial cost"]
total[6] <- global[rownames(global) == "system (grid) system total internal initial cost"]
total[7] <- global[rownames(global) == "system (grid) system total discounted cost"]
total[8] <- global[rownames(global) == "system (grid) system total discounted demand"]
total[9] <- global[rownames(global) == "system (grid) system total discounted cost"] / global[rownames(global) == "system (grid) system total discounted demand"]


total <- as.numeric(total)
per_HH <- total/total[3]
per_HH[c(1,3,9)] <- NA
grid.table <- data.frame(indicator,units,total,per_HH)

# output final result by grid.type
write.csv(mini.grid.table, "mini_grid_result.csv", row.names=F)
write.csv(off.grid.table, "off_grid_result.csv", row.names=F)
write.csv(grid.table, "grid.table.csv", row.names=F)



#Append total & Column percentage
pct <- as.data.frame(prop.table(as.matrix(local_agg[,2:ncol(local_agg)]),2))
colnames(pct) <- paste("percentage of", colnames(pct), sep=": ")

name_vec <- names(local_agg)[1]
for (i in 1:ncol(pct))
{
    name_vec <- c(name_vec, names(local_agg)[2:ncol(local_agg)][i], names(pct)[i])    
}
local_agg_output <- cbind(local_agg,pct)
local_agg_output <- local_agg_output[,name_vec]
local_agg_output["Total", 2:ncol(local_agg_output)] <- colSums(local_agg_output[, 2:ncol(local_agg_output)])
local_agg_output[,"Metric...System"] <- as.character(local_agg_output[,"Metric...System"])
local_agg_output[5, "Metric...System"] <- "Total"
rownames(local_agg_output) <- NULL