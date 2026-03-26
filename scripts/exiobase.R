### Labour embodied in exports ###

library(Matrix)
library(dplyr)

path <- "/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_2020_pxp/"


e <- readRDS(paste0(path, 'satellite/F.rds'))
l <- (e[16:21,]) # hours worked in millions

# Creating the leontief inverse
path1 <- "/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/"

"IOT_2020_pxp"