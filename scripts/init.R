
library(Matrix)

setwd("~/projects/co2-wealth-inequality")
path1 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
path2 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/E"
new <- "/mnt/nfs_fineprint/tmp/gloria/v057new/MRIO"

# loading labels
io <- read.csv(paste0(path1, "/labels.csv"))
fd <- read.csv(paste0(path1, "/labels_fd.csv"))

# defining index
index <- which(io$type == "i")

# general settings
years <- 1995:2020
year <- 1995
co2 <- 1842


for (year in years) {

  E <-
    read.csv(paste0(path2, "/20230310_120secMother_AllCountries_002_TQ-Results_", year, "_057_Markup001(full).csv"), header = F)
  saveRDS(E[co2,], paste0(path2, "/co2/co2_total_edgar_", year, ".rds"))
}
rm(E, co2)



# getting IO size objects and saving them
for (year in years) {
  # loading data sets
  co2 <- readRDS(paste0(path2, "/co2/co2_total_edgar_", year, ".rds")) %>%
    .[, index] %>% as.numeric()
  co2[is.na(co2)] <- 0
  saveRDS(co2, file = paste0(new, "/co2_edgar_", year, ".rds"))
  rm(co2)
  
  load(paste0(path1, "/x_", year, ".RData"))
  x <- x[index]
  x[is.na(x)] <- 0
  x <- x + 0.0001
  saveRDS(x, file = paste0(new, "/x_", year, ".rds"))
  rm(x)
  
  load(paste0(path1, "/L_inv_", year, ".RData"))
  L_inv <- L_inv[index, index]
  L_inv[is.na(L_inv)] <- 0
  saveRDS(L_inv, file = paste0(new, "/L_", year, ".rds"))
  rm(L_inv)
  
  load(paste0(path1,"/Y_", year, ".RData"))
  Y <- Y[grep("p", rownames(Y)), ]
  Y[is.na(Y)] <- 0
  saveRDS(Y, file = paste0(new, "/Y_", year, ".rds"))
  rm(Y)
  
  gc()
  print(year)
}
