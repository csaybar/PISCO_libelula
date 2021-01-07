#' Complete data using PISCOp 2.2

# 1. Load utils functions -------------------------------------------------
library(sf)
library(xts)
library(qmap)
library(RCurl)
library(gstat)
library(raster)
library(R.utils)
library(hydroGOF)
library(magrittr)
library(gdalUtils)
library(tidyverse)
library(lubridate)

source("src/utils.R")
options(max.print=1000)

# 2. Global Parameters ----------------------------------------------------
chirp_path <- "/home/csaybar/CHIRP" #Path with CHIRP in netcdf
daily_chirp <- list.files(paste0(chirp_path,"/daily"), full.names = TRUE)
montly_chirp <- list.files(paste0(chirp_path,"/monthly"), full.names = TRUE)


path <- "/home/csaybar/"
create_folders(path)
spatial_databases <- load_dataset2(path) # gauge rain data
rg_codes <- spatial_databases$metadata$V_COD_ESTA

# 3. Complete monthly data ------------------------------------------------
for (index in seq_along(rg_codes)) {
  sp_data <- create_spatial_dataset2(
    path = path,
    rg_code = rg_codes[index],
    spatial_databases = spatial_databases,
    step = "monthly"
  )

  # Complete with cutoff!
  completed_cutoff_rg <- complete_CUTOFF_m(path, sp_data, spatial_databases)

  # If not possible use Qm
  sat_value <- multi_extract(montly_chirp, completed_cutoff_rg)
  completed_qm_rg <- complete_qm_m(path, sat_value, sp_data)

  # From sp to data.frame
  final_dataset <- cbind(id = rg_codes[index],
                         coordinates(completed_qm_rg),
                         completed_qm_rg@data)
  if (index == 1) {
    final_dataset_merge <- final_dataset
  } else {
    final_dataset_merge <-  rbind(final_dataset_merge, final_dataset)
  }
}

# write.csv(final_dataset_merge, "/home/csaybar/month.csv")

# 4. Complete daily data ------------------------------------------------
for (index in seq_along(rg_codes)) {
  sp_data <- create_spatial_dataset2(
    path = path,
    rg_code = rg_codes[index],
    spatial_databases = spatial_databases,
    step = "daily"
  )

  # Complete with cutoff!
  completed_cutoff_rg <- complete_CUTOFF_d(path, sp_data, spatial_databases)

  # If not possible use Qm
  sat_value <- multi_extract(daily_chirp, completed_cutoff_rg)
  completed_qm_rg <- complete_qm_d(path, sat_value, sp_data)

  # From sp to data.frame
  final_dataset <- cbind(id = rg_codes[index],
                         coordinates(completed_qm_rg),
                         completed_qm_rg@data)
  if (index == 1) {
    final_dataset_merge <- final_dataset
  } else {
    final_dataset_merge <-  rbind(final_dataset_merge, final_dataset)
  }
}
# write.csv(final_dataset_merge, "/home/csaybar/day.csv")
