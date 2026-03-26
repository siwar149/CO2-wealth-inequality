
library(Matrix)
library(data.table)
library(tidyverse)
library(readxl)
library(parallel)

path <- "/mnt/nfs_fineprint/tmp/gloria/v059-compiled/"

io <- readRDS('~/projects/AFD/rds/label_IO.rds')

l <- read_excel('~/projects/bio-carbon/Data/fd-labels.xlsx')
cn <- regmatches(l$labels, gregexpr("\\([^()]+\\)", l$labels))
cn <- sapply(cn, function(x) if(length(x) >= 2) x[2] else x[1])
cn <- gsub("[()]", "", cn)
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

countries <- unique(io[,1])

years <- 1990:2021
year <- 1990

# Precompute once
row_country <- io[, 1]

results_all <- vector("list", length(years))
names(results_all) <- as.character(years)

for (year in years) {
  l <- readRDS(paste0(path, "TQ/TQ_", year, ".rds"))
  x <- readRDS(paste0(path, "X/X_", year, ".rds"))
  x <- x + 1e-8
  L <- readRDS(paste0(path, "L/L_", year, ".rds"))
  Y <- readRDS(paste0(path, "Y/Y_", year, ".rds"))
  
  # direct labour coefficients
  e <- colSums(l[368:369, , drop = FALSE]) / x
  
  # aggregate final demand to destination countries
  colnames(Y) <- cn
  Y <- agg(Y)   # expected: sectors x countries
  
  # embodied labour multipliers by source sector
  MP <- sweep(L, 1, e, "*")   # clearer than e * L
  
  # aggregate source sectors to source countries BEFORE final multiplication
  MP_country <- rowsum(MP, group = row_country, reorder = FALSE)
  
  # country x country matrix:
  # embodied labour from source country in destination-country final demand
  F_country <- MP_country %*% Y
  
  # exports = total foreign final demand = row sum minus diagonal
  exp_vals <- rowSums(F_country) - diag(F_country)
  
  results_all[[as.character(year)]] <- data.table(
    iso   = rownames(F_country),
    year  = year,
    unit  = "ppl",
    value = exp_vals
  )
}

results_all <- rbindlist(results_all)

write.csv(results_all, '~/projects/co2-wealth-inequality/output/embodied-labour.csv')


# VA per worker
calc_va_per_worker_exports <- function(year, path, cn, agg, row_country) {
  l <- readRDS(file.path(path, "TQ", paste0("TQ_", year, ".rds")))
  x <- readRDS(file.path(path, "X",  paste0("X_",  year, ".rds")))
  x <- x + 1e-8
  L <- readRDS(file.path(path, "L",  paste0("L_",  year, ".rds")))
  Y <- readRDS(file.path(path, "Y",  paste0("Y_",  year, ".rds")))
  V <- readRDS(file.path(path, "V",  paste0("V_",  year, ".rds")))
  
  # VA per worker coefficients
  V <- colSums(V) / colSums(l[368:369, , drop = FALSE])
  e <- V / x
  
  # aggregate final demand to destination countries
  colnames(Y) <- cn
  Y <- agg(Y)   # expected: sectors x countries
  
  # embodied labour multipliers by source sector
  MP <- sweep(L, 1, e, "*")
  
  # aggregate source sectors to source countries
  MP_country <- rowsum(MP, group = row_country, reorder = FALSE)
  
  # country x country matrix
  F_country <- MP_country %*% Y
  
  # exports = foreign final demand
  exp_vals <- rowSums(F_country) - diag(F_country)
  
  data.table(
    iso   = rownames(F_country),
    year  = year,
    unit  = "VA/worker",
    value = exp_vals
  )
}

cl <- makeCluster(6)

clusterExport(cl, c("path", "cn", "agg", "row_country", "calc_va_per_worker_exports"))
clusterEvalQ(cl, {
  library(data.table)
  NULL
})

results_all <- parLapply(
  cl,
  years,
  function(year) calc_va_per_worker_exports(year, path, cn, agg, row_country)
)

stopCluster(cl)

names(results_all) <- as.character(years)


results_all <- rbindlist(results_all)

write.csv(results_all, '~/projects/co2-wealth-inequality/output/embodied-va_worker.csv')


install.packages("WDI")
library(WDI)

d1 <- read.csv('~/projects/co2-wealth-inequality/output/embodied-va_worker.csv')
d2 <- read.csv('~/projects/co2-wealth-inequality/output/embodied-labour.csv')

pop <- WDI(
  country = "all",
  indicator = "SP.POP.TOTL",
  start = 1990,
  end = 2021
)

pop <- pop %>% select(iso3c, year, SP.POP.TOTL)

d1 <- d1 %>% left_join(pop, by = c('iso'='iso3c','year')) %>% mutate(pc = value / SP.POP.TOTL)
d2 <- d2 %>% left_join(pop, by = c('iso'='iso3c','year')) %>% mutate(pc = value / SP.POP.TOTL)


write.csv(d1, '~/projects/co2-wealth-inequality/output/embodied-va_worker.csv', row.names = F)
write.csv(d2, '~/projects/co2-wealth-inequality/output/embodied-labour.csv', row.names = F)
