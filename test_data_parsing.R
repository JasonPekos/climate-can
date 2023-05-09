library(ggplot2)
library(raster)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)
library(targets)
theme_set(theme_void())

getmean <- function(raster, time) {
  # Extract the raster layer at the specified time
  layer <- raster[[time]]

  # Calculate the average value of all cells in the layer
  avg <- global(layer, fun = mean, na.rm = TRUE)

  # Return the average value
  return(avg)
}

# Future
url_future <- "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP2.6/monthly_ens/absolute/CMIP5_rcp2.6_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc" # nolint

# Historical
url_hist <- "https://dd.weather.gc.ca/climate/cmip5/netcdf/historical/monthly_ens/absolute/CMIP5_hist_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc" # nolint

download.file(url_future, dest = "future.ncdf", method = "wget")
download.file(url_hist, dest = "hist.ncdf", method = "wget")

# Make raster objects
tas_future <- rast("future.ncdf")
tas_hist <- rast("hist.ncdf")


# Load ON Census regions
on <- tar_read(raw_geom_data_on) %>%
  st_as_sf() %>%
  st_transform(crs = crs(tas_hist))

kenora <- on %>% filter(Region.Name == "Kenora, Unorganized (NO)")

# Crop tas_future around dundas
tk <- crop(tas_future, kenora)
tas_future_kenora <- mask(tk, kenora)

tas_future_kenora


# Plot
ggplot(kenora) +
  geom_spatraster(data = tas_future_kenora[[20]]) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º")
  ) +
  geom_spatvector(fill = NA)
