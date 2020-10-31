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
create_folders(path)
init_date <- "2015-01-01"
last_date <- "2015-12-01"

# Run PISCOp -------------------------------------------------------
run_PISCOp(path, init_date, last_date)
