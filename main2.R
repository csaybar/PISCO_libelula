#' Create a complete dataset for PISCOp 2.2
#
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
source("utils.R")
options(max.print=1000)

# Global Parameters -------------------------------------------------------
path <- "/home/csaybar/"
month <- as.Date("1981-01-01")

# Global Parameters -------------------------------------------------------
monthly_data <- ""
daily_data <- ""
run_PISCOp(path = path, month = month)
