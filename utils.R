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
  mgauge <- senamhi_gauge_data$monthly
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

    ## If the specific rain gauge larger than 10 years?
    if (senamhi_gauge_one_length < min_month) {
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

    # If not any rain gauge accomplish the requirements, stop the function
    if (is.null(selected_raingauges)) {
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
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
    month_ratios_db[[raingauge_code]] <- list(cm = cm_data,
                                              rm = rm_data,
                                              raingauges = cor_station_code)
  }
  month_ratios_db
}


# dgauge is the daily dataset
cutoff_dataset_creator_d <- function(dgauge) {
  dgauge <- senamhi_gauge_data$daily
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
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
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
      month_ratios_db[[raingauge_code]] <- list(cm = rep(1,12),
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
    print("done")
    daily_ratios_db[[raingauge_code]] <- list(cm = cm_data,
                                              rm = rm_data,
                                              raingauges = cor_station_code)
  }
  daily_ratios_db
}

cutoff_dataset_creator <- function(senamhi_gauge_data) {
  senamhi_gauge_data <- download_senamhi_data(path)
  monthly_ratios <- cutoff_dataset_creator_m(senamhi_gauge_data$monthly)
  daily_ratios <- cutoff_dataset_creator_d(senamhi_gauge_data$daily)
  cutoff_ratios <- list(cutoff_monthly = monthly_ratios, cutoff_daily = daily_ratios)
  gauge_data <- sprintf("%s/data/senamhi_cutoff_ratios.RData", path)
  save(cutoff_ratios, file = gauge_data)
}
