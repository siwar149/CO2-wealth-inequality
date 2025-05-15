
library(Matrix)
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

# paths
path1 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
path2 <- "/mnt/nfs_fineprint/tmp/gloria/v057new/E"

# new metal data and gloria satellites
s <- read_excel('input/GLORIA_ReadMe_057.xlsx', sheet = 'Satellites')
m <- read.csv('input/GLORIAv057_mat_ext_new.csv', sep = ';')

# getting the index of materials
m$Tonnes[7] <- 'Iron ores'
mat <- which(s$Sat_indicator %in% m$Tonnes)
which(m$Tonnes %in% s$Sat_indicator)

# loading labels
io <- read.csv(paste0(path1, "/labels.csv"))
index <- which(io$type == "i")

# general settings
years <- 1995:2020
year <- 1994


for (year in years) {
  E <-
    read.csv(paste0(path2, "/20230310_120secMother_AllCountries_002_TQ-Results_", year, "_057_Markup001(full).csv"), header = F)
  saveRDS(E[mat, index], paste0(path2, "/materials/mat_", year, ".rds"))
}

rm(E)

io2 <- io %>%
  filter(type == 'i')

sat <- readRDS(paste0(path2, '/materials/mat_2020.rds'))
rownames(sat) <- s$Sat_indicator[mat]

deu <- sat[, which(io2$Region_acronyms == 'DEU')]

rowSums(deu)
sum(rowSums(deu) == 0)

# the shares matrix
mp<-deu / rowSums(deu)

# handle 'Other metallic ores'
io3 <- io %>%
  filter(type == 'i' & Region_acronyms == 'DEU')

mp[rownames(mp) == 'Other non-metallic minerals n.e.c.',]<-0
mp[rownames(mp) == 'Other non-metallic minerals n.e.c.', which(io3$Sector_names == 'Other non-metallic mineral products n.e.c.')] <- 1
sum(mp[rownames(mp) == 'Other non-metallic minerals n.e.c.',])

# changing order of the new materials of germany
rownames(m) <- m$Tonnes
colnames(m)[1] <- 'materials'
m2 <- m[rownames(mp),] 

m2 <- m2 %>%
  pivot_longer(cols = colnames(m2)[-1]) %>%
  rename(year = name) %>%
  mutate(year = as.numeric(str_replace(year, "X", "")))

# getting the new extentions  
ext <- function(x) {x <- mp * (m2 %>% filter(year == x) %>% .$value); return(x)}

new_ext <- lapply(1994:2020, ext)

names(new_ext) <- 1994:2020


# replace the german part on the full extentions
sat <- readRDS(paste0(path2, '/materials/mat_2020.rds'))
sat[, which(io2$Region_acronyms == 'DEU')] <- new_ext[["2020"]]

new_sat <- function(x) {
  sat <- readRDS(paste0(path2, '/materials/mat_',x,'.rds'))
  sat[, which(io2$Region_acronyms == 'DEU')] <- new_ext[[as.character(x)]]
  return(sat)}

satellites <- lapply(1994:2020, new_sat)
names(satellites) <- 1994:2020
saveRDS(satellites, paste0(path2, "/materials/new_mat_DEU.rds"))


