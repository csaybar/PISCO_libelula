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
month <- as.Date("1981-01-01")
create_folders(path)

# Global Parameters -------------------------------------------------------
run_PISCOp_m(path = path, month = month)
