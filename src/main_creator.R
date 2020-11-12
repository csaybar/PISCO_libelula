#' Create a complete dataset for PISCOp 2.2

# 1. Load utils functions
library(sf)
library(xts)
library(qmap)
library(RCurl)
library(raster)
library(R.utils)
library(magrittr)
library(tidyverse)
library(lubridate)
source("src/utils.R")
options(max.print=1000)

# 2. Define a path to save final and intermediate files
path <- "/home/csaybar/"
day_seq <- seq(as.Date("1981-01-01"), as.Date("2019-12-31"), "day")
month_seq <- seq(as.Date("1981-01-01"), as.Date("2019-12-31"), "month")

# 3. Create folders
create_folders(path)

# 4. Download CHIRPd and CHIRPm
for (index in seq_along(day_seq)) {
  download_CHIRPd(date = day_seq[index], path)
}
for (index in seq_along(day_seq)) {
  download_CHIRPm(date = month_seq[index], path)
}

# 5.Test if all the necessary files are saved
test_CHIRP(path)

# 6. Create data cubes
create_CHIRPd_netcdf2(path)
create_CHIRPm_netcdf(path)

# 7. Download SENAMHI dataset
download_senamhi_data(path)

# 8. Create Rm and Cm for cutoff
# See: CUTOFF: A spatio-temporal imputation method (2015)
cutoff_dataset_creator(path)

# 9. Create quantile models for each station
qm_dataset_creator(path)
