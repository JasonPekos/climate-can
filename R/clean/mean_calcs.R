library(terra)
library(tidyterra)
library(sf)

getmean_raster <- function(raster, time) {
  # Extract the raster layer at the specified time
  layer <- raster[[time]]

  # Calculate the average value of all cells in the layer
  avg <- global(layer, fun = mean, na.rm = TRUE)

  # Return the average value
  return(avg)
}

getmean_geouid <- function(country_raster, census_geoms, geouid, time){

    # Filter the census geoms to the specified geouid
    census_region <- census_geoms %>%
        st_as_sf() %>%
        st_transform(crs = crs(country_raster)) %>%
        filter(GeoUID == geouid)

    # Crop and mask the raster to the census region
    raster <- crop(country_raster, census_region) %>%
        mask(census_region)

    # Extract the raster layer at the specified time
    layer <- country_raster[[time(country_raster) == time]]

    # Calculate the average value of all cells in the layer
    avg <- global(layer, fun = mean, na.rm = TRUE)

    # Return the average value
    return(avg)
}

modify_time_labels <- function(raster) {
  time_values <- terra::time(raster)
  time_labels <- as.Date(time_values)
  
  new_time_labels <- as.Date(format(time_labels, "%Y-%m-01"))
  terra::time(raster) <- new_time_labels
  
  return(raster)
}

