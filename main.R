library(raster)
library(lubridate)
library(RCurl)
library(R.utils)

source("utils.R")

path = "/home/csaybar/" # path to save data
day_seq <- seq(as.Date("1981-01-01"), as.Date("2019-07-31"), "day")
month_seq <- seq(as.Date("1981-01-01"), as.Date("2019-07-31"), "month")

# 1. Create folders
create_folders(path)

# 2. Download CHIRPd
for (index in seq_along(day_seq)) {
  download_CHIRPd(date = day_seq[index], path)
}

# 2. Download CHIRPm
for (index in seq_along(day_seq)) {
  download_CHIRPm(date = month_seq[index], path)
}




