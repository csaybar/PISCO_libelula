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
  step = "daily"
)
check_timeserie(sp_data, col = "black", main = "raw data")

# 5. Get time serie
daily_chirp <- list.files("/home/csaybar/CHIRP/daily/",full.names = TRUE)
sat_value <- multi_extract(daily_chirp, sp_data)


# 5. Complete missing values using CUTOFF
final_sp <- complete_time_series_d(
  path = path,
  sp_data = sp_data,
  spatial_databases = spatial_databases,
  sat_value = sat_value,
  method = "CUTOFF+QM"
)

check_timeserie(final_sp, col = "red", main = "CUTOFF+QM")
check_timeserie(sp_data, col = "black", main = "raw data", add = TRUE)
