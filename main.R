library(raster)
library(lubridate)
library(RCurl)
library(magrittr)
library(R.utils)

source("utils.R")

path = "/home/csaybar/" # path to save data
day_seq <- seq(as.Date("1981-01-01"), as.Date("2020-07-31"), "day")
month_seq <- seq(as.Date("1981-01-01"), as.Date("2020-07-31"), "month")

# 1. Create folders
create_folders(path)

# 2. Download CHIRPd and CHIRPm
for (index in seq_along(day_seq)) {
  download_CHIRPd(date = day_seq[index], path)
}
for (index in seq_along(day_seq)) {
  download_CHIRPm(date = month_seq[index], path)
}

# 4.Test if all the necessary files are saved
test_CHIRP(path)

# 5. Create data cubes
create_CHIRPd_netcdf2(path) 
create_CHIRPm_netcdf(path)



