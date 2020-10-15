#' Create a complete dataset for PISCOp 2.2
#

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

# 2. Global Parameters -------------------------------------------------------
path <- "/home/piscop/"
month <- as.Date("2015-11-01")
spatial_databases <- load_dataset(path) # gauge rain data
create_folders(path)

# Run PISCOp -------------------------------------------------------
sp_data <- create_spatial_dataset(path, month, spatial_databases)
run_PISCOp_m(path = path, sp_data = sp_data$monthly)
run_PISCOp_d(path = path, sp_data = sp_data$daily)

