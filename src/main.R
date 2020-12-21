#' Create a complete dataset for PISCOp 2.2

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
path <- "/home/csaybar/"
month <- as.Date("2018-11-01")
create_folders(path)
spatial_databases <- load_dataset2(path) # gauge rain data

# Run PISCOp -------------------------------------------------------
sp_data <- create_spatial_dataset(path, month, spatial_databases)
codigos <- read.csv("/home/csaybar/Downloads/metodo.csv")
complete_methods <- codigos$metodo
to_delete <- codigos$V_COD_ESTA[which(codigos$metodo == "A")]

run_PISCOp_m(path = path, sp_data = sp_data$monthly,
             complete_series = complete_methods, to_delete = to_delete)

run_PISCOp_d(path = path, sp_data = sp_data$daily,
             complete_series = complete_methods, to_delete = to_delete)
