library(Matrix)
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

# paths
path <- "/mnt/nfs_fineprint/tmp/gloria/v057new/MRIO"
path1 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
path2 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/E"

# loading labels
io <- read.csv(paste0(path1, "/labels.csv"))
fd <- read.csv(paste0(path1, "/labels_fd.csv"))

# general settings
years <- 1995:2020
year <- 1995
country <- 'DEU'

# some referential data
m <- read.csv('input/GLORIAv057_mat_ext_new.csv', sep = ';')
s <- read_excel('input/GLORIA_ReadMe_057.xlsx', sheet = 'Satellites')
io2 <- io %>%
  filter(type == 'i')

# old footprints

library(parallel)


process_year <- function(year) {
  cat("Processing year:", year, "\n")
  mat <- readRDS(paste0(path2, '/materials/mat_', year, '.rds'))
  x <- readRDS(paste0(path, "/x_", year, ".rds"))
  L <- readRDS(paste0(path, "/L_", year, ".rds"))
  Y <- readRDS(paste0(path, "/Y_", year, ".rds"))
  
  rbindlist(lapply(rownames(mat), function(row) {
    cat("  Processing material:", row, "\n")
    e <- as.numeric(mat[row, ] / x)
    MP <- e * L
    de <- sum(mat[, which(io2$Region_acronyms == 'DEU')][row, ])
    imp <- colSums(MP[which(io2$Region_acronyms != 'DEU'), ] %*% rowSums(Y[, which(fd$Region_acronyms == 'DEU')]))
    exp <- colSums(MP[which(io2$Region_acronyms == 'DEU'), ] %*% rowSums(Y[, which(fd$Region_acronyms != 'DEU')]))
    data.table(
      material = row,
      year = year,
      unit = 'tonnes',
      DE = de,
      IMP = imp,
      EXP = exp
    )
  }))
}

# Detect number of available cores
n_cores <- parallel::detectCores() # 12

# Parallel execution
results_list <- mclapply(years, process_year, mc.cores = 6)
results <- rbindlist(results_list)
results$material <- as.numeric(results$material)

results <- results %>%
  mutate(RMC = DE + IMP - EXP) %>%
  left_join(s[which(s$Sat_indicator %in% m$Tonnes), c(1,3)], by = c('material' = 'Lfd_Nr')) %>%
  select(Sat_indicator, year, unit, DE, IMP, EXP, RMC) %>%
  rename(material = Sat_indicator)

write.csv(results, 'output/original-DEU-footprint.csv')

## New footprints ##
sat <- readRDS(paste0(path2, "/materials/new_mat_DEU.rds"))


process_year <- function(year) {
  cat("Processing year:", year, "\n")
  mat <- sat[[as.character(year)]]
  x <- readRDS(paste0(path, "/x_", year, ".rds"))
  L <- readRDS(paste0(path, "/L_", year, ".rds"))
  Y <- readRDS(paste0(path, "/Y_", year, ".rds"))
  
  rbindlist(lapply(rownames(mat), function(row) {
    cat("  Processing material:", row, "\n")
    e <- as.numeric(mat[row, ] / x)
    MP <- e * L
    de <- sum(mat[, which(io2$Region_acronyms == 'DEU')][row, ])
    imp <- colSums(MP[which(io2$Region_acronyms != 'DEU'), ] %*% rowSums(Y[, which(fd$Region_acronyms == 'DEU')]))
    exp <- colSums(MP[which(io2$Region_acronyms == 'DEU'), ] %*% rowSums(Y[, which(fd$Region_acronyms != 'DEU')]))
    data.table(
      material = row,
      year = year,
      unit = 'tonnes',
      DE = de,
      IMP = imp,
      EXP = exp
    )
  }))
}

# Parallel execution
results_list <- mclapply(years, process_year, mc.cores = 6)
results2 <- rbindlist(results_list)
results2$material <- as.numeric(results2$material)

results2 <- results2 %>%
  mutate(RMC = DE + IMP - EXP) %>%
  left_join(s[which(s$Sat_indicator %in% m$Tonnes), c(1,3)], by = c('material' = 'Lfd_Nr')) %>%
  select(Sat_indicator, year, unit, DE, IMP, EXP, RMC) %>%
  rename(material = Sat_indicator)

write.csv(results2, 'output/new-DEU-footprint.csv')
