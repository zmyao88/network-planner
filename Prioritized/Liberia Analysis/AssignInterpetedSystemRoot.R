

## jonathans dropbox directory 
setwd("~/Dropbox/WB/Liberia/Modeling/2013-05-06/NPOutputs/3X-200Max-N360")
rm(list=ls())


##I. - Import the master merged dataset of metrics local with bins and MV length assigned
master <- read.csv("MasterMerged_20130628.csv")

#II. - Group nodes to master system by root
#develop a system column
master$Geographic.System.Phase1 <- NA
master$Geographic.System.Phase2 <- NA
master$Geographic.System.Phase3 <- NA

# 1- Mano Substation 
master$Geographic.System.Phase1[master$root %in% c(1,
                                                   2,
                                                   3)] <- "ManoSubstation"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "ManoSubstation"] <- "ManoSubstation"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "ManoSubstation"] <- "ManoSubstation"

# 2-Tubmanburg
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-2")] <- "Tubmanburg MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Tubmanburg MuniGrid"] <- "Tubmanburg MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Tubmanburg MuniGrid"] <- "Monrovia"

# 3- Monrovia Substation 
master$Geographic.System.Phase1[master$root %in% c(10, 11, 12, 13, 14, 15,
                                                   4, 5, 6, 7, 8, 9,
                                                   "MuniGrid-1",
                                                   "MuniGrid-5",
                                                   "MuniGrid-6")] <- "Monrovia System"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Monrovia System"] <- "Monrovia System"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Monrovia System"] <- "Monrovia System"

# 4- Buchanan Substation 
master$Geographic.System.Phase1[master$root %in% c(16,
                                                   17,
                                                   18,
                                                   "MuniGrid-30")] <- "Buchanan Substation"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "ManoSubstation"] <- "Buchanan Substation"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "ManoSubstation"] <- "Buchanan Substation"

# 5- Harbel MuniGrid 
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-12",
                                                   "MuniGrid-17",
                                                   "MuniGrid-20")] <- "Harbel MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Harbel MuniGrid"] <- "Monrovia System"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Harbel MuniGrid"] <- "Monrovia System"


# 6- Kakata MuniGrid 
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-16"
                                                   )] <- "Kakata MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Kakata MuniGrid"] <- "Monrovia System"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Kakata MuniGrid"] <- "Monrovia System"


# 7- Bong Mines 
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-14")] <- "Bong Mines MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Bong Mines MuniGrid"] <- "Bong Mines MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Bong Mines MuniGrid"] <- "Monrovia System"


# 8- Yogbo  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-36")] <- "Yogbo MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Yogbo MuniGrid"] <- "Yogbo Mines MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Yogbo MuniGrid"] <- "Monrovia System"

# 9- Kpayeakwe MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-25",
                                                   "MuniGrid-35",
                                                   "MuniGrid-33")] <- "Kpayeakwe MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Kpayeakwe MuniGrid"] <- "Yogbo Mines MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Kpayeakwe MuniGrid"] <- "Monrovia System"

# 10- Suakoko MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-40")] <- "Suakoko MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Suakoko MuniGrid"] <- "Yogbo Mines MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Suakoko MuniGrid"] <- "Monrovia System"

# 11- Gbarnga MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-41")] <- "Gbarnga MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Gbarnga MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Gbarnga MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"

# 12- Yekepa Substation/Guinea Cross Border  
master$Geographic.System.Phase1[master$root %in% c(100, 103, 105, 106, 107, 109, 110,
                                                   112, 113, 114, 115, 116, 118, 119,
                                                   120, 121, 123, 125, 127, 129, 131,
                                                   133, 136, 137, 138, 139, 142, 144,
                                                   146, 148, 149, 151, 152, 155, 156, 157,
                                                   158, 159, 160, 162, 163, 
                                                   20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                                                   30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                                                   40, 41, 42, 43, 44, 45, 47, 49,
                                                   50, 54, 61,
                                                   70, 72, 73, 74,
                                                   81, 82, 84, 86, 88, 90, 91, 95, 97,
                                                   "MuniGrid-47",
                                                   "MuniGrid-50",
                                                   "MuniGrid-52")] <- "Yekepa Substation/Guinea Cross Border"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Yekepa Substation/Guinea Cross Border"] <- "Yekepa Substation/Guinea Cross Border"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Yekepa Substation/Guinea Cross Border"] <- "Yekepa Substation/Guinea Cross Border"

# 13- Zorzor MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-42")] <- "Zorzor MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Zorzor MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Zorzor MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"

# 14- Voinjama MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-38")] <- "Voinjama MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Voinjama MuniGrid"] <- "Voinjama MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Voinjama MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"

# 14- Foya Town MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-10",
                                                   "MuniGrid-22",
                                                   "MuniGrid-27")] <- "Foya Town MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Foya Town MuniGrid"] <- "Foya Town MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Foya Town MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"


# 15- Kungbor MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-8")] <- "Kungbor MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Kungbor MuniGrid"] <- "Foya Town MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Kungbor MuniGrid"] <- "Yekepa Substation/Guinea Cross Border"

# 16- Zwedru / Cote dIvoire Cross Border  
master$Geographic.System.Phase1[master$root %in% c(101, 102, 104, 108, 111, 117,
                                                   122, 124, 126, 128,
                                                   130, 132, 134, 135, 
                                                   140, 141, 143, 145, 147,
                                                   150, 153, 154, 161, 164, 165, 166, 167, 168, 169,
                                                   170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
                                                   180,
                                                   48, 51, 52, 53, 55, 56, 57, 58, 59,
                                                   60, 62, 63, 64, 65, 66, 67, 68, 69,
                                                   71, 75, 76, 77, 78, 79, 
                                                   80, 83, 85, 87, 89, 
                                                   92, 93, 94, 96, 98, 99,
                                                   "MuniGrid-48")] <- "Zwedru / Cote dIvoire Cross Border"
#Phases 2 and 3 on grid
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Zwedru / Cote dIvoire Cross Border"] <- "Zwedru / Cote dIvoire Cross Border"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Zwedru / Cote dIvoire Cross Border"] <- "Zwedru / Cote dIvoire Cross Border"


# 17- Ziah Town MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-54")] <- "Ziah Town MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Ziah Town MuniGrid"] <- "Ziah Town MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Ziah Town MuniGrid"] <- "Harper / Cote DIvoire Cross Border"

# 18- Kanweaken MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-53")] <- "Kanweaken MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Kanweaken MuniGrid"] <- "Kanweaken MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Kanweaken MuniGrid"] <- "Harper / Cote DIvoire Cross Border"

# 19- Greenville MuniGrid  
master$Geographic.System.Phase1[master$root %in% c("MuniGrid-45")] <- "Greenville MuniGrid"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Greenville MuniGrid"] <- "Greenville MuniGrid"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Greenville MuniGrid"] <- "Monrovia System"

# 20- Harper Grid  
master$Geographic.System.Phase1[master$root %in% c(181, 182, 183, 184, 185, 186, 187, 189,
                                                   190, 191, 192, 194, 195, 196, 197, 198, 199,
                                                   200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
                                                   210, 211, 212, 213, 214, 215, 216, 217, 218,
                                                   "MuniGrid-53",
                                                   "MuniGrid-56",
                                                   "MuniGrid-57")] <- "Harper / Cote DIvoire Cross Border"
#Phases 2 and 3 still remain primarily on substation  
master$Geographic.System.Phase2[master$Geographic.System.Phase1 %in% "Harper / Cote DIvoire Cross Border"] <- "Harper / Cote DIvoire Cross Border"
master$Geographic.System.Phase3[master$Geographic.System.Phase1 %in% "Harper / Cote DIvoire Cross Border"] <- "Harper / Cote DIvoire Cross Border"


##III. Output csv now with systems defined 
write.csv(master, "MasterMerged_20130701_DefinedSystems.csv", row.names=F)

                            