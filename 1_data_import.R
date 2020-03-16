#increase memory limt
memory.limit(24000)
# load csv files
# cps_03_07 <- read.csv("Data/Data Raw/cps_03_07.csv")
# cps_08_12 <- read.csv("Data/Data Raw/cps_08_12.csv")
# cps_13_17 <- read.csv("Data/Data Raw/cps_13_17.csv")
cps_05_09 <- read.csv("Data/Data Raw/cps_00020.csv")
cps_10_14 <- read.csv("Data/Data Raw/cps_00021.csv")
cps_15_19 <- read.csv("Data/Data Raw/cps_00022.csv")

# remove diffhear, diffeye, diffphys, paidemployeey
# combine csv files
#cps <- rbind(cps_03_07, cps_08_12, cps_13_17)
cps <- rbind(cps_05_09,cps_10_14,cps_15_19)

# save data
save(cps_05_09, file = "Data/Data Raw/cps_05_09.RData")
save(cps_10, file = "Data/Data Raw/cps_10.RData")
save(cps_15, file = "Data/Data Raw/cps_15.RData")
save(cps, file = "Data/Data Raw/cps.RData")


