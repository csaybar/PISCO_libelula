print.sf <- function(x) {
  geoms = which(vapply(x, function(col) inherits(col, "sfc"),
                       TRUE))
  nf = length(x) - length(geoms)
  app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
  if (any(!is.na(st_agr(x))))
    app = paste0(app, "\n", "Attribute-geometry relationship: ",
                 summarize_agr(x))
  if (length(geoms) > 1)
    app = paste0(app, "\n", "Active geometry column: ",
                 attr(x, "sf_column"))
  print(st_geometry(x), n = 0, what = "Simple feature collection with",
        append = app)
}

create_folders <- function(path) {
  dir.create(sprintf('%s/data/', path), showWarnings = FALSE)
  # CHIRPM
  dir.create(sprintf('%s/data/CHIRPM/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRPM/CHIRPd/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRPM/CHIRPm/', path), showWarnings = FALSE)

  # CHIRP
  dir.create(sprintf('%s/data/CHIRP_0.05/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.05/CHIRPd/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/CHIRP_0.05/CHIRPm/', path), showWarnings = FALSE)

  # PISCOp
  dir.create(sprintf('%s/data/PISCOp/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/PISCOp/PISCOpd/', path), showWarnings = FALSE)
  dir.create(sprintf('%s/data/PISCOp/PISCOpm/', path), showWarnings = FALSE)
}


download_CHIRPm <- function(date,
                            path,
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
  chirp_crop
}


download_CHIRPd <- function(date,
                            path,
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
  chirp_crop
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

download_monthly_data <- function() {
  path <- tempdir()
  # Donwload metedata
  master_data_path <- "https://drive.google.com/uc?id=1BwQmaWzkGIqcedzDer-5i-j5KNTUkQ-E&export=download"
  master_output <- sprintf('%s/senamhi_metadata.csv', path)
  download.file(master_data_path, master_output)
  metadata <- read.csv(master_output, stringsAsFactors = FALSE) %>%
    '['(c("V_COD_ESTA", "Y_LONGITUD", "X_LATITUD"))

  # Download monthly data
  monthly_data_path <- "https://drive.google.com/uc?id=19qc8UMtfwbLuhoVMJKyb5VpNVaLJBq-S&export=download"
  monthly_output <- sprintf('%s/senamhi_monthly_data.csv', path)
  download.file(monthly_data_path, monthly_output)
  monthly_data <- read.csv(monthly_output, stringsAsFactors = FALSE)
  dates <- sprintf("date_%s", format(as.Date(monthly_data[[1]]), "%Y%m%d"))

  for (index in seq_along(colnames(monthly_data)[-1])) {
    # test if column in monthly_data and row in metadata are the same
    if(colnames(monthly_data[index + 1]) == sprintf("X%s",metadata[index,1])) {
      coordinates <- st_point(as.numeric(metadata[index,2:3]))
      dates_col <- monthly_data[[index + 1]] %>%
        matrix(nrow = 1) %>%
        data.frame() %>%
        `colnames<-`(dates)
      cod_station <- data.frame(V_COD_ESTA = metadata[index,1])
      final_dataset <- cbind(cod_station, dates_col)
      final_dataset_sf <- st_sf(final_dataset, geom = st_sfc(coordinates))
      st_crs(final_dataset_sf) <- 4326

      if (index == 1) {
        monthly_dataset <- final_dataset_sf
      } else {
        monthly_dataset[index,] <- final_dataset_sf
      }
    } else {
      stop("Code station are not the same!")
    }
  }
  monthly_dataset
}

download_daily_data <- function() {
  path <- tempdir()
  # Donwload metedata
  master_data_path <- "https://drive.google.com/uc?id=1BwQmaWzkGIqcedzDer-5i-j5KNTUkQ-E&export=download"
  master_output <- sprintf('%s/senamhi_metadata.csv', path)
  download.file(master_data_path, master_output)
  metadata <- read.csv(master_output, stringsAsFactors = FALSE) %>%
    '['(c("V_COD_ESTA", "Y_LONGITUD", "X_LATITUD"))

  # Download monthly data
  daily_data_path <- "https://drive.google.com/uc?id=1KwIrK5c4bqq2mYhL8Uz9wg693K0hSnjJ&export=download"
  daily_output <- sprintf('%s/senamhi_monthly_data.csv', path)
  download.file(daily_data_path, daily_output)
  daily_data <- read.csv(daily_output, stringsAsFactors = FALSE)
  dates <- sprintf("date_%s", format(as.Date(daily_data[[1]]), "%Y%m%d"))

  for (index in seq_along(colnames(daily_data)[-1])) {
    # test if column in monthly_data and row in metadata are the same
    if(colnames(daily_data[index + 1]) == sprintf("X%s",metadata[index,1])) {
      coordinates <- st_point(as.numeric(metadata[index,2:3]))
      dates_col <- daily_data[[index + 1]] %>%
        matrix(nrow = 1) %>%
        data.frame() %>%
        `colnames<-`(dates)
      cod_station <- data.frame(V_COD_ESTA = metadata[index,1])
      final_dataset <- cbind(cod_station, dates_col)
      final_dataset_sf <- st_sf(final_dataset, geom = st_sfc(coordinates))
      st_crs(final_dataset_sf) <- 4326
      if (index == 1) {
        daily_dataset <- final_dataset_sf
      } else {
        daily_dataset[index,] <- final_dataset_sf
      }
    } else {
      stop("Code station are not the same!")
    }
  }
  daily_dataset
}

download_senamhi_data <- function(path) {
  gauge_data <- sprintf("%s/data/senamhi_gauge_data.RData", path)
  if (file.exists(gauge_data)) {
    load(gauge_data)
  } else {
    senamhi_m_data <- download_monthly_data()
    senamhi_m_data_sp <- as(senamhi_m_data, "Spatial")
    senamhi_d_data <- download_daily_data()
    senamhi_d_data_sp <- as(senamhi_d_data, "Spatial")
    data_list <- list(daily = senamhi_d_data_sp, monthly = senamhi_m_data_sp)
    save(data_list, file = gauge_data)
  }
  invisible(data_list)
}

# mgauge is the monthly dataset
cutoff_dataset_creator_m <- function(mgauge) {
  month_ratios_db <- list()
  for (index  in seq_len(nrow(mgauge))) {
    raingauge_code <- as.character(mgauge[index, 1][[1]])
    # 1. ~100 km buffers
    ## create a simple buffer
    senamhi_gauge_one <- mgauge[index, ]
    roi_g <- buffer(senamhi_gauge_one, 100000)

    ## Pick up rain gauges inside the buffer
    senamhi_gauge_selected <- rgeos::intersect(mgauge, roi_g)
    removed_one <- which(senamhi_gauge_selected$V_COD_ESTA  == senamhi_gauge_one$V_COD_ESTA)
    senamhi_gauge_selected <- senamhi_gauge_selected[-removed_one,]

    # If there is no any rain gauges NA will be assigned
    if (length(senamhi_gauge_selected) == 0) {
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }

    # 2. at least 10 years and cor > 0.8
    min_month <- 12*10

    ## data available for the specific rain gauge
    senamhi_gauge_one_d <- senamhi_gauge_one[-1] %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(starts_with("date"))
    senamhi_gauge_one_dates <- senamhi_gauge_one_d %>%
      as.numeric() %>%
      'names<-'(colnames(senamhi_gauge_one_d)) %>%
      na.omit() %>%
      names()
    senamhi_gauge_one_length <- senamhi_gauge_one_dates %>%
      length()

    ## Is the specific rain gauge larger than 10 years?
    if (senamhi_gauge_one_length < min_month) {
      # If not, NA will be assigned
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }

    ## Wrangling data from rain gauges inside the buffer
    selected_data_available <- senamhi_gauge_selected[-1] %>%
      as.data.frame() %>%
      as_tibble() %>%
      '['(senamhi_gauge_one_dates)

    ## select only rain station with more than 10 years at common and cor > 0.8
    selected_raingauges <- NULL
    for (index in seq_len(nrow(selected_data_available))) {
      selected_data_available_l <- selected_data_available[index,] %>%
        as.numeric()
      # more than 10 years?
      condition1 <- length(na.omit(selected_data_available_l)) > min_month

      # cor > 0.8?
      data_gauge_one <- as.numeric(senamhi_gauge_one_d[senamhi_gauge_one_dates])
      cor_b_gauges <- cor(selected_data_available_l,
                          data_gauge_one,
                          use = "pairwise.complete.obs")
      condition2 <- cor_b_gauges > 0.8
      if (condition1 & condition2) {
        selected_raingauges <- append(selected_raingauges, index)
      }
    }

    # If not any rain gauge accomplish the requirements, assign NA and next! :)
    if (is.null(selected_raingauges)) {
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }

    senamhi_gauge_selected_final <- senamhi_gauge_selected[selected_raingauges, ]
    cor_station_code <- senamhi_gauge_selected_final$V_COD_ESTA

    # Calculating Cm
    # Cm -> Monthly mean value for an specific rain gauge.
    vector <- sprintf("%02d", 1:12)
    cm_data <- NULL
    for (index in vector) {
      month_data <- senamhi_gauge_one_d [
        which(substring(colnames(senamhi_gauge_one_d), 10,11) == index)
      ] %>% as.numeric() %>% mean(na.rm=TRUE)
      cm_data <- append(cm_data, month_data)
    }

    # Calculating Rm
    # Rm -> Monthly mean value for the group of station of a specific rain gauge.
    senamhi_gauge_selected_final[-1] %>%
      as.data.frame() %>%
      as_tibble() %>% apply(2, function(x) mean(x, na.rm = TRUE)) ->
      final_partialrm
    rm_data <- NULL
    for (index in vector) {
      month_data <- final_partialrm [
        which(substring(names(final_partialrm), 10,11) == index)
      ] %>% as.numeric() %>% mean(na.rm=TRUE)
      rm_data <- append(rm_data, month_data)
    }
    month_ratios_db[[raingauge_code]] <- list(cm = cm_data,
                                              rm = rm_data,
                                              raingauges = cor_station_code)
  }
  month_ratios_db
}


# dgauge is the daily dataset
cutoff_dataset_creator_d <- function(dgauge) {
  daily_ratios_db <- list()
  for (index  in seq_len(nrow(dgauge))) {
    print(index)
    raingauge_code <- as.character(dgauge[index, 1][[1]])
    # 1. ~100 km buffers
    ## create a simple buffer
    senamhi_gauge_one <- dgauge[index, ]
    roi_g <- buffer(senamhi_gauge_one, 100000)

    ## Pick up rain gauges inside the buffer
    senamhi_gauge_selected <- rgeos::intersect(dgauge, roi_g)
    removed_one <- which(senamhi_gauge_selected$V_COD_ESTA  == senamhi_gauge_one$V_COD_ESTA)
    senamhi_gauge_selected <- senamhi_gauge_selected[-removed_one,]

    if (length(senamhi_gauge_selected) == 0) {
      daily_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }

    # 2. at least 10 years and cor > 0.8
    min_day <- 12*365

    ## data available for the specific rain gauge
    senamhi_gauge_one_d <- senamhi_gauge_one[-1] %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(starts_with("date"))
    senamhi_gauge_one_dates <- senamhi_gauge_one_d %>%
      as.numeric() %>%
      'names<-'(colnames(senamhi_gauge_one_d)) %>%
      na.omit() %>%
      names()
    senamhi_gauge_one_length <- senamhi_gauge_one_dates %>%
      length()

    ## If the specific rain gauge larger than 10 years?
    if (senamhi_gauge_one_length < min_day) {
      daily_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }

    ## Wrangling data from rain gauges inside the buffer
    selected_data_available <- senamhi_gauge_selected[-1] %>%
      as.data.frame() %>%
      as_tibble() %>%
      '['(senamhi_gauge_one_dates)

    ## select only rain station with more than 10 years at common and cor > 0.8
    selected_raingauges <- NULL
    for (index in seq_len(nrow(selected_data_available))) {
      selected_data_available_l <- selected_data_available[index,] %>%
        as.numeric()
      # more than 10 years?
      condition1 <- length(na.omit(selected_data_available_l)) > min_day

      # cor > 0.5?
      data_gauge_one <- as.numeric(senamhi_gauge_one_d[senamhi_gauge_one_dates])
      cor_b_gauges <- cor(selected_data_available_l,
                          data_gauge_one,
                          use = "pairwise.complete.obs")
      condition2 <- cor_b_gauges > 0.5
      if (condition1 & condition2) {
        selected_raingauges <- append(selected_raingauges, index)
      }
    }

    # If not any rain gauge accomplish the requirements, stop the function
    if (is.null(selected_raingauges)) {
      daily_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
                                                rm = rep(1,12),
                                                raingauges = NA)
      next
    }
    senamhi_gauge_selected_final <- senamhi_gauge_selected[selected_raingauges, ]
    cor_station_code <- senamhi_gauge_selected_final$V_COD_ESTA
    # Calculating Cm
    vector <- sprintf("%02d", 1:12)
    cm_data <- NULL
    for (index in vector) {
      month_data <- senamhi_gauge_one_d [
        which(substring(colnames(senamhi_gauge_one_d), 10,11) == index)
      ] %>% as.numeric() %>% mean(na.rm=TRUE)
      cm_data <- append(cm_data, month_data)
    }
    # Calculating Rm
    senamhi_gauge_selected_final[-1] %>%
      as.data.frame() %>%
      as_tibble() %>% apply(2, function(x) mean(x, na.rm = TRUE)) ->
      final_partialrm
    rm_data <- NULL
    for (index in vector) {
      month_data <- final_partialrm [
        which(substring(names(final_partialrm), 10,11) == index)
      ] %>% as.numeric() %>% mean(na.rm=TRUE)
      rm_data <- append(rm_data, month_data)
    }
    daily_ratios_db[[raingauge_code]] <- list(cm = cm_data,
                                              rm = rm_data,
                                              raingauges = cor_station_code)
  }
  daily_ratios_db
}

cutoff_dataset_creator <- function(path) {
  senamhi_gauge_data <- download_senamhi_data(path)
  message("Creating CUTOFF monthly parameters ... please wait")
  monthly_ratios <- cutoff_dataset_creator_m(senamhi_gauge_data$monthly)
  message("Creating CUTOFF daily parameters ... please wait")
  daily_ratios <- cutoff_dataset_creator_d(senamhi_gauge_data$daily)
  cutoff_ratios <- list(cutoff_monthly = monthly_ratios, cutoff_daily = daily_ratios)
  gauge_data <- sprintf("%s/data/senamhi_cutoff_ratios.RData", path)
  save(cutoff_ratios, file = gauge_data)
}

# PISCOp v2.1 function
qmap_set_m <- function(obs_ts, gridded_ts,date) {
  obs_ts <- xts(obs_ts, date)
  gridded_ts <- xts(gridded_ts, date)
  qm_fit <- fitQmapQUANT(coredata(obs_ts),
                         coredata(gridded_ts) ,
                         qstep = 0.01,
                         nboot = 1,
                         wet.day = TRUE,
                         type = "tricube")
  qm_fit
}

# PISCOp v2.1 function
qmap_set_d <- function(obs_ts, gridded_ts, date) {
  obs_ts <- xts(obs_ts, date)
  gridded_ts <- xts(gridded_ts, date)
  qm_fit <- fitQmapQUANT(coredata(obs_ts),
                         coredata(gridded_ts) ,
                         qstep = 0.01, nboot = 1, wet.day = T, type = "tricube")
  qm_fit
}


qm_daily_creator <- function(path) {
  # 1. Load rain gauge data
  senamhi_gauge_data <- download_senamhi_data(path)
  daily_gauge_data <- senamhi_gauge_data$daily
  day_seq <- as.Date(names(daily_gauge_data)[-1], "date_%Y%m%d")

  # 2. Read netcdf data
  sat_data <- path %>%
    sprintf("%sdata", .) %>%
    list.files(pattern = "CHIRPd", full.names = TRUE) %>%
    lapply(brick)

  # 3. Quantile time!
  qm_raingauge_list <- list()
  for (index in seq_len(length(daily_gauge_data))) {
    rain_gauge <- daily_gauge_data[index,]
    rain_gauge_data <- rain_gauge@data[, -1] %>% as.numeric()
    rain_gauge_name <- rain_gauge@data[, 1] %>% as.character()

    ## Extract sat data
    batch_extract <- function(x) raster::extract(x, rain_gauge) %>% as.numeric()
    sat_gauge_data <- lapply(sat_data, batch_extract) %>% unlist()

    ## Fit quantile map according to PISCOp
    qm_raingauge_list[[rain_gauge_name]] <- qmap_set_d(rain_gauge_data, sat_gauge_data, day_seq)
  }
  qm_raingauge_list
}

qm_month_creator <- function(path, month_seq) {
  # 1. Load rain gauge data
  senamhi_gauge_data <- download_senamhi_data(path)
  monthly_gauge_data <- senamhi_gauge_data$monthly
  month_seq <- as.Date(names(monthly_gauge_data)[-1], "date_%Y%m%d")

  # 2. Read netcdf data
  sat_data <- brick(sprintf("%s/data/CHIRPm_1981_2019.nc", path))

  # 3. Quantile time!
  qm_raingauge_list <- list()
  for (index in seq_len(length(monthly_gauge_data))) {
    rain_gauge <- monthly_gauge_data[index,]
    rain_gauge_data <- rain_gauge@data[, -1] %>% as.numeric()
    rain_gauge_name <- rain_gauge@data[, 1] %>% as.character()

    ## Extract sat data
    sat_gauge_data <- raster::extract(sat_data, rain_gauge) %>% as.numeric()

    ## Fit quantile map according to PISCOp
    qm_raingauge_list[[rain_gauge_name]] <- qmap_set_m(rain_gauge_data, sat_gauge_data, month_seq)
  }
  qm_raingauge_list
}

qm_dataset_creator <- function(path) {
  qm_rg_daily_list <- qm_daily_creator(path)
  qm_rg_month_list <- qm_month_creator(path)
  qm_list <- list(qm_monthly = qm_rg_month_list, qm_daily = qm_rg_daily_list)
  gauge_data <- sprintf("%s/data/qm_models.RData", path)
  save(qm_list, file = gauge_data)
}

run_cutoff_m <- function(pp_values, cutoff_ratios) {
  # 1. Load date and values
  date <- as.Date(names(pp_values), format = "date_%Y%m%d")
  values <- pp_values[[1]]

  # 2. Load CUTOFF metadata
  monthly_cutoff <- cutoff_ratios$cutoff_monthly
  rain_gauges_code <- names(monthly_cutoff)

  # 3. Trying to complete the data if it is possible
  monthx <- lubridate::month(date)
  new_values <- rep(NA, length(values))
  for (index in seq_along(values)) {
    val <- values[index]
    # This rain gauge have a group of rain gauges (>10 years & cor>0.8),
    # if not return the same value.
    if(any(is.na(monthly_cutoff[[index]][["raingauges"]]))) {
      new_value <- val
    } else {
      if (is.na(val)) {
        cutoff_group <- monthly_cutoff[[rain_gauges_code[index]]]
        group_position <- which(rain_gauges_code %in% cutoff_group$raingauges)
        R <- values[group_position] %>% mean(na.rm = TRUE)
        # na_position <- group_position[values[group_position] %>% is.na ]
        R_m <- monthly_cutoff[[index]][["rm"]][monthx]
        C_m <- monthly_cutoff[[index]][["cm"]][monthx]
        new_value <- R*C_m/R_m
      } else {
        new_value <- val
      }
    }
    new_values[[index]] <- new_value
  }
  new_values
}

#' qmap_get: BIAS CORRECTED CHIRP
#' - sat: CHIRP time series.
#' - model: Qmap bias model
qmap_get_m <- function(sat, model) {
  qmap::doQmapQUANT(sat, model, type="tricub") %>% as.numeric() %>% round(., 2)
}

run_qm_model <- function(pp_values, sat_values, qm_list) {
  values <- pp_values[[1]]
  missing_data <- which(is.na(values))
  for (index in missing_data) {
    values[index] <- suppressWarnings(
      qmap_get_m(sat_values[index], qm_list[[index]])
    )
  }
  values
}

run_PISCOp_m <- function(path, month = "1981-01-01") {
  # Wrangling dates
  month <- as.Date(month)
  # days <- paste0(
  #   format(month, "%Y-%m-"),
  #   month %>% days_in_month() %>% seq_len() %>% sprintf("%02d", .) # number of days
  # ) %>% as.Date()

  # 1. Download satellite images
  chirpx_m <- download_CHIRPm(date = month, path = path)
  #chirpx_m <- raster("/home/csaybar/data/CHIRP_0.05/CHIRPm/CHIRP.1981.01.tif")
  # chirpx_d <- mapply(download_CHIRPd, days, MoreArgs = list(path = path)) %>%
  #   stack()

  # 2. Complete rain gauge
  load(sprintf("%s/data/senamhi_gauge_data.RData", path))
  load(sprintf("%s/data/senamhi_cutoff_ratios.RData", path))
  load(sprintf("%s/data/qm_models.RData", path))
  rg_data_brute <- rg_data
  rg_data <- data_list$monthly[format(month, "date_%Y%m%d")]
  message(sprintf("Number of NA in Brute data: %s", sum(is.na(rg_data$date_19810101))))

  # 3. Run Monthly CUTOFF
  test_number_rg(rg_data)
  rg_data[[1]] <- run_cutoff_m(pp_values = rg_data, cutoff_ratios = cutoff_ratios)
  rg_data[[1]][is.nan(rg_data[[1]])] <- NA
  message(sprintf("Number of NA after CUTOFF: %s", sum(is.na(rg_data$date_19810101))))

  # 4. Run Monthly Qm
  sat_data <- raster::extract(chirpx_m, rg_data)
  rg_data[[1]] <- run_qm_model(
    pp_values = rg_data,
    sat_values = sat_data,
    qm_list = qm_list$qm_monthly
  )
  message(sprintf("Number of NA after Qm: %s", sum(is.na(rg_data$date_19810101))))

  # 5. CHIRPM
  chirpm_m <- suppressMessages(
    create_chirm_m(chirp_m = chirpx_m, path = path, month = month)
  )
  writeRaster(
    x = chirpm_m,
    filename = sprintf(
      "%s/data/CHIRPM/CHIRPm/CHIRPMm.%s.tif", path, format(month, "%Y.%m.%d")
    ),
    overwrite = TRUE
  )

  # 6. Mean double station
  rg_data_ds <- suppressWarnings(
    mean_double_Station(gauge = rg_data, sat = chirpm_m, longlat = TRUE)
  )

  # 7. Interpolation
  CHIRPMm_log <- log1p(chirpm_m)
  names(CHIRPMm_log) <- "sat"
  names(rg_data_ds) <- "gauge"
  rg_data_ds$gauge <- log1p(rg_data_ds$gauge)
  kd <- ROK(gauge = rg_data_ds, cov = CHIRPMm_log)
  kd <- expm1(kd)
  if(sum(getValues(kd) > 10000, na.rm = TRUE) > 0) {
    kd <- RIDW(
      gauge = rg_data_ds,
      cov = CHIRPMm_log,
      formula = "gauge~sat"
    )
    kd <- expm1(kd)
  }

  # Stats for PISCOp model
  pbias_p <- hydroGOF::pbias(
    sim = raster::extract(kd, rg_data_brute),
    obs = rg_data_brute[[1]]
  )
  cor_p <- cor(
    x = raster::extract(kd, rg_data_brute),
    y = rg_data_brute$date_19810101,
    use = "pairwise.complete.obs"
  )
  message(sprintf("PBIAS: %s && PEARSON: %s", pbias_p, round(cor_p,2)))
  kd[kd < 0] = 0
  writeRaster(
    x = kd,
    filename = sprintf(
      "%s/data/CHIRPM/CHIRPm/PISCOpm.%s.tif", path, format(month, "%Y.%m.%d")
    ),
    overwrite = TRUE
  )
}

test_number_rg <- function(pp_values) {
  if (length(pp_values) != 447 ) {
    stop(
      "PISCOp v2.2 needs to have specifically 447 rain gauges.",
      "If you are interested in a new version with more rain gauges",
      "run main_creator.R an follow the instructions."
    )
  }
}

piscop_base <- function() {
  piscop_extend <- raster::extent(-81.3, -68, -18.8, 1)
  piscop_base_p <- raster(piscop_extend, nrows=198, ncols=133)
  crs(piscop_base_p) <- "+proj=longlat +datum=WGS84 +no_defs"
  piscop_base_p[] <- seq_len(ncell(piscop_base_p))
  piscop_base_p
}

resampleR <- function (R1 = NULL, R2 = NULL, t_res = NULL, t_ext = NULL, r = "cubicspline") {
  Rb <- R1
  if(inMemory(R1)){
    nameR1 <- sprintf("%s/%s.tif",getwd(),names(R1))
    R1 <- writeRaster(R1,nameR1,overwrite=T)
  }
  if (!missing(R2)) {
    t1 <- c(xmin(R2), ymin(R2), xmax(R2), ymax(R2))
    t2 <- c(res(R2)[1], res(R2)[2])
  } else {
    t1 <-  t_ext
    t2 <-  t_res
  }

  Routput2 <- sprintf("%s/R_%s.tif",getwd(),names(R1))
  Rf <- gdalUtils::gdalwarp(srcfile = R1@file@name, dstfile = Routput2,
                 tr = t2, te = t1, output_Raster = T, overwrite = T,
                 verbose = T,r=r) * 1

  file.remove(Routput2)
  if (inMemory(Rb)) file.remove(nameR1)
  Rf
}

create_chirm_m <- function(chirp_m, path, month) {
  # 1. Global params
  e_lumb <- 0.5
  l_month <- lubridate::month(month)
  piscopb <- piscop_base()

  # 2. From 0.05 to 0.1
  chirpx_R <- resampleR(R1 = chirp_m, R2 = piscopb)*1

  # 3. Read PISCOp_clim and CHIRPMm
  pisco_clim <- list.files(
    path = sprintf('%s/data/PISCOpclim', path),
    full.names = TRUE
  )[l_month] %>% raster::raster()
  chp_clim <- list.files(
    path = sprintf('%s/data/CHPclim', path),
    full.names = TRUE
  )[l_month] %>% raster::raster()

  # 4. Create CHIRPMm
  CHIRPMm <- chirpx_R * ((pisco_clim + e_lumb)/(chp_clim + e_lumb))
  CHIRPMm[CHIRPMm < 0] = 0
  CHIRPMm
}

mean_double_Station <- function(gauge = NULL, sat = NULL, longlat = TRUE) {
  # I deeply apologize for this script I made it 3 years ago :'v

  # 1.  Convert raster to points
  projection(sat) <- projection(gauge)
  point <- data.frame(rasterToPoints(sat))
  colnames(point) <- c("x", "y", "sat")
  coordinates(point) <- ~x + y
  projection(point) <- projection(sat)

  # 2. Distance from rain gauges to sat points
  distances <- function(x, ptsat = point){
    which.min(spDists(ptsat, gauge[x, ], longlat = T))
  }
  loc <- do.call("c", lapply(1:length(gauge), distances))
  duplicates <- loc[which(duplicated(loc))]

  # gauge (sp) to data.frame (gauge2p)
  gauge2 <- cbind(coordinates(gauge), gauge@data)
  colnames(gauge2) <- c("x", "y", colnames(gauge2)[3:length(gauge2)])
  gauge2p <- gauge2

  list <- lapply(1:length(duplicates), function(i) {
    dupliStation <- which(loc == duplicates[i])
    gaugeD <- gauge2p[dupliStation, ]
    PromStation <- colMeans(gaugeD, na.rm = T)
    list(Prom = PromStation, position = dupliStation)
  })

  stat <- do.call("rbind", lapply(1:length(duplicates), function(x) list[[x]]$position))
  stat2 <- do.call("rbind", lapply(1:length(duplicates), function(x) list[[x]]$Prom))
  gauge2p[stat[, 1], ] <- stat2
  newG <- gauge2p[-stat[, 2], ]
  coordinates(newG) <- ~x + y
  projection(newG) <- projection(sat)
  return(newG)
}
