create_folders <- function(path) {
  dir.create(sprintf('%s/data/', path), showWarnings = FALSE)
  # chrips diario
  dir.create(sprintf('%s/data/CHIRP_0.1/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.1/CHIRPd/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.1/CHIRPm/', path), showWarnings = FALSE)

  dir.create(sprintf('%s/data/CHIRP_0.05/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.05/CHIRPd/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.05/CHIRPm/', path), showWarnings = FALSE)
}


download_CHIRPm <- function(date,
                            set_dir = getwd(),
                            BBlonMin = -86,
                            BBlonMax = -66,
                            BBlatMin = -19.25,
                            BBlatMax = 1.25
) {
  tmpfile <- tempfile()
  chirp_monthly_path <- sprintf("%sdata/CHIRP_0.05/CHIRPm/", path)
  server <- "https://data.chc.ucsb.edu/products/CHIRP/monthly/"  #ftp data
  chirp_file <- sprintf("%sCHIRP.%s.tif", server, format(date,"%Y.%m"))
  download.file(chirp_file, tmpfile, mode = "wb")
  chirp_crop <- crop(raster(tmpfile), c(BBlonMin, BBlonMax, BBlatMin, BBlatMax) )
  chirp_monthly_file <- paste0(chirp_monthly_path, basename(chirp_file))
  writeRaster(chirp_crop, chirp_monthly_file, overwrite = TRUE)
  unlink(tmpfile)
}


download_CHIRPd <- function(date,
                            set_dir = getwd(),
                            BBlonMin = -86,
                            BBlonMax = -66,
                            BBlatMin = -19.25,
                            BBlatMax = 1.25) {
  chirps_daily_path <- sprintf("%s/data/CHIRP_0.05/CHIRPd/", path)
  server <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05"  #ftp data

  chirp_file <- sprintf("%s/%s/chirps-v2.0.%s.tif.gz",
                        server,
                        year(date),
                        format(date,"%Y.%m.%d"))
  tmpfile <- paste0(tempfile(),".tif.gz")
  download.file(chirp_file, tmpfile, mode = "wb")
  unzipfile <- gunzip(tmpfile)
  r1 <- raster(unzipfile)
  chirp_crop <- crop(r1, c(BBlonMin, BBlonMax, BBlatMin, BBlatMax) )
  chirp_crop[chirp_crop < 0] = 0
  file_name <- gsub("\\.gz", "", basename(chirp_file))
  writeRaster(x = chirp_crop,
              filename = paste0(chirps_daily_path, file_name),
              overwrite = TRUE)
  unlink(unzipfile)
}
