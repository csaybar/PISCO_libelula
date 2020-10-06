#' Create a complete dataset for PISCOp 2.2
#
# 1. Load utils functions
library(sf)
library(xts)
library(qmap)
library(RCurl)
library(gstat)
library(raster)
library(R.utils)
library(magrittr)
library(gdalUtils)
library(tidyverse)
library(lubridate)
source("utils.R")
options(max.print=1000)

# Global Parameters -------------------------------------------------------
path <- "/home/csaybar/"
month <- as.Date("2015-10-01")
create_folders(path)

# Global Parameters -------------------------------------------------------
sp_data <- create_spatial_dataset(path, month)
run_PISCOp_m(path = path, sp_data = sp_data$monthly)
run_PISCOp_d(path = path, sp_data = sp_data$daily)

