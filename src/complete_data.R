path <- "/home/csaybar/"
month <- as.Date("2015-11-01")
create_folders(path)
spatial_databases <- load_dataset2(path) # gauge rain data

sp_data <- create_spatial_dataset2(path, 117006, spatial_databases)


raingauge <- 117006
piscop_complete_m <- function(path, raingauge, sat) {
  load(sprintf("%s/data/senamhi_cutoff_ratios.RData", path))
  load(sprintf("%s/data/qm_models.RData", path))

}

piscop_complete_d <- function(raingauge, sat, ratios, models) {

}

test_number_rg(rg_data)
rg_data[[1]] <- run_cutoff_m(pp_values = rg_data, cutoff_ratios = cutoff_ratios)
rg_data[[1]][is.nan(rg_data[[1]])] <- NA
message(sprintf("Number of NA after CUTOFF: %s", sum(is.na(rg_data[[1]]))))

# 3. Run Monthly CUTOFF
test_number_rg(rg_data)
rg_data@data <- run_cutoff_d(pp_values = rg_data, cutoff_ratios = cutoff_ratios)
message(sprintf("Number of NA after CUTOFF: %s", sum(is.na(rg_data@data))))

# 4. Run Monthly Qm
sat_data <- raster::extract(chirpx_d, rg_data)
for (qm_index in seq_along(days)) {
  rg_data[[qm_index]] <- run_qm_model(
    pp_values = rg_data[qm_index],
    sat_values = sat_data[,qm_index],
    qm_list = qm_list$qm_daily
  )
}
