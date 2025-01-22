
path1 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
path <- "/mnt/nfs_fineprint/tmp/gloria/v057new/MRIO"

# loading labels
io <- read.csv(paste0(path1, "/labels.csv"))
fd <- read.csv(paste0(path1, "/labels_fd.csv"))

# defining index
index <- which(io$type == "i")

# general settings
years <- 1995:2020
year <- 1995

results_all <- list()

for (year in years) {
  # loading data sets
  co2 <- readRDS(paste0(path, "/co2_edgar_", year, ".rds"))
  x <- readRDS(paste0(path, "/x_", year, ".rds"))
  e <- co2 / x
  L <- readRDS(paste0(path, "/L_", year, ".rds"))
  Y <- readRDS(paste0(path, "/Y_", year, ".rds"))
  
  # multipliers
  MP <- e * L
  # footprints
  FP <- MP %*% Y
  colnames(FP) <- paste0(fd$Region_acronyms, "_", fd$Final_demand_names)
  FP <- colSums(FP)
  results <- data.table(country = substr(names(FP),1,3),
                        cc = substr(names(FP), 5,100),
                        year = year,
                        unit = "kilotonnes",
                        value = FP)
  
  results_all[[as.character(year)]] <- results
  
  print(year)
  
}

results <- rbindlist(results_all)
results <- results %>% filter(value != 0)
saveRDS(results, file = "./output/footprint-co2_excl_short_cycl_org_c.rds")
rm(list = ls())
gc()
