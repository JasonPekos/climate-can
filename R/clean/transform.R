transform_df <- function(data, geo_data, filter_date, high_temp, low_temp, high_pcp, low_pcp){
  
  ## Compute masks 
  # Temp masks
  high_temp_mask <- mapply(
    getmask_geouid,
    MoreArgs = list(
      climate_raster = unwrap(high_temp),
      census_geoms = geo_data
    ),
    geouid = unique(data$GeoUID)
  )
  
  low_temp_mask <- mapply(
    getmask_geouid,
    MoreArgs = list(
      climate_raster = unwrap(low_temp),
      census_geoms = geo_data
    ),
    geouid = unique(data$GeoUID)
  )
  
  # Precipitation masks
  high_pcp_mask <- mapply(
    getmask_geouid,
    MoreArgs = list(
      climate_raster = unwrap(high_pcp),
      census_geoms = geo_data
    ),
    geouid = unique(data$GeoUID)
  )
  
  low_pcp_mask <- mapply(
    getmask_geouid,
    MoreArgs = list(
      climate_raster = unwrap(low_pcp),
      census_geoms = geo_data
    ),
    geouid = unique(data$GeoUID)
  )
  
  data_transformed <- data %>%
    filter(Date < filter_date) %>%
    mutate(
      mean_temp_high = mapply(getmean_geouid, masked_raster = high_temp_mask, time = Date),
      mean_temp_low = mapply(getmean_geouid, masked_raster = low_temp_mask, time = Date),
      mean_pcp_high = mapply(getmean_geouid, masked_raster = high_pcp_mask, time = Date),
      mean_pcp_low = mapply(getmean_geouid, masked_raster = low_pcp_mask, time = Date),
      tot_prod = rowMeans(dplyr::select(., starts_with("production")), na.rm = TRUE),
      t = rep(seq_len(length(unique(Date))), times = length(unique(GeoUID))),
      s = rep(seq_len(length(unique(GeoUID))), each = length(unique(Date)))
    )
  
  return(data_transformed)
}


