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

create_CHIRPd_netcdf <- function(path, years = 1981:2019) {
  chirps_list <- list()
  chirp_path <- sprintf("%s/data/CHIRP_0.05/CHIRPd/", path)
  output <- sprintf("%s/data/", path)
  cf <-  sprintf("%s/CHIRPd_%s_%s.nc", output, years[1], years[length(years)])
  
  chirps_raster <- list.files(path = chirp_path,
                              pattern = "\\.tif$", 
                              full.names = TRUE, 
                              recursive=TRUE) %>% 
    raster::stack() %>% 
    raster::brick() 
  writeRaster(
    x = chirps_raster,
    filename =  cf,
    overwrite = TRUE
  )
}


create_CHIRPm_netcdf <- function(path, years = 1981:2019) {
  chirps_list <- list()
  chirp_path <- sprintf("%s/data/CHIRP_0.05/CHIRPm/", path)
  output <- sprintf("%s/data/", path)
  cf <-  sprintf("%s/CHIRPd_%s_%s.nc", output, years[1], years[length(years)])
  
  chirps_raster <- list.files(path = chirp_path,
                              pattern = "\\.tif$", 
                              full.names = TRUE, 
                              recursive=TRUE) %>% 
    raster::stack() %>% 
    raster::brick() 
  writeRaster(
    x = chirps_raster,
    filename =  cf,
    overwrite = TRUE
  )
}

create_CHIRPd_netcdf2 <- function(path, years = 1981:2019) {
  #  necessary paths
  chirp_path <- sprintf("%s/data/CHIRP_0.05/CHIRPd/", path)
  output <- sprintf("%s/data/", path)
  
  for (year in years) {
    # List all files by year and create a datacube
    chirps_raster <- list.files(path = chirp_path,
                                pattern = year %>% as.character(), 
                                full.names = TRUE, 
                                recursive=TRUE) %>% 
      raster::stack() %>% 
      raster::brick() 
    
    # Test if exist the expected number of files
    test_length_by_years(x = chirps_raster, year)
    
    # Output name
    cf <-  sprintf("%s/CHIRPd_%s.nc", output, year)
    
    # Save it!
    message(sprintf("Saving CHIRPd netcdf year: %s", year))
    writeRaster(
      x = chirps_raster,
      filename =  cf,
      overwrite = TRUE
    ) 
  }
}


# Fast Testing!
test_chirpd <- function(path, years) {
  chirp_path <- sprintf("%s/data/CHIRP_0.05/CHIRPd/", path) 
  init_year <- as.Date(sprintf("%s-01-01", years[1]))
  last_year <- as.Date(sprintf("%s-12-31", years[length(years)]))
  expected_dates <- seq(init_year, last_year, "day")
  chirps_raster <- list.files(path = chirp_path,
                              pattern = "\\.tif$", 
                              full.names = TRUE, 
                              recursive=TRUE) 
  if (length(expected_dates) != length(chirps_raster)) {
    stop("CHIRPd files are imcompleted!")
  }
}

test_chirpm <- function(path, years) {
  chirp_path <- sprintf("%s/data/CHIRP_0.05/CHIRPm/", path) 
  init_year <- as.Date(sprintf("%s-01-01", years[1]))
  last_year <- as.Date(sprintf("%s-12-31", years[length(years)]))
  expected_dates <- seq(init_year, last_year, "month")
  chirps_raster <- list.files(path = chirp_path,
                              pattern = "\\.tif$", 
                              full.names = TRUE, 
                              recursive=TRUE) 
  if (length(expected_dates) != length(chirps_raster)) {
    stop("CHIRPm files are imcompleted!")
  }
}

#' Test is exist 
test_length_by_years <- function(x, year) {
  nlyrs <- nlayers(x)
  if (leap_year(year)) {
    y_length <- 366
  } else {
    y_length <- 365
  }
  condition <- nlyrs  ==  y_length
  if (!condition) {
    stop(sprintf("Missing files!\nActual:%s\nExpected:%s", nlyrs, y_length))
  } 
}

test_CHIRP <- function(path, years = 1981:2019) {
  test_chirpd(path, years)
  test_chirpm(path, years)
}