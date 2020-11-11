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

# 2. Global Parameters -------------------------------------------------------
path <- "/home/csaybar/"
create_folders(path)
spatial_databases <- load_dataset2(path) # gauge rain data

# 3. Pick up a rain gauge randomly
rg_code <- spatial_databases$metadata$V_COD_ESTA[sample(447, 1)]

# 4. Create a sp object (use step = "daily" for complete rainfall time series)
sp_data <- create_spatial_dataset2(
  path = path,
  rg_code = rg_code,
  spatial_databases = spatial_databases,
  step = "monthly"
)

# 5. Complete missing values using CUTOFF
completed_cutoff_rg <- complete_CUTOFF_m(path, sp_data, spatial_databases)

# 5. Complete missing values using Quantile Mapping
# Donwload rain sat data here:
daily_chirp <- list.files("/home/csaybar/CHIRP/monthly/",full.names = TRUE)
sat_value <- multi_extract(daily_chirp, completed_cutoff_rg)
completed_qm_rg <- complete_qm_m(path, sat_value, sp_data)

plot(as.numeric(completed_qm_rg@data),type="l")
