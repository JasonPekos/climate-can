library(targets)
library(parallel)

t_low <- unwrap(tar_read(raw_cmip5_future_low))
t_med <- unwrap(tar_read(raw_cmip5_future_med))
t_high <- unwrap(tar_read(raw_cmip5_future_high))

num_cores <- detectCores() - 1

prod_on <- tar_read(raw_prod_data_on)
cmip5_med <- unwrap(tar_read(cmip5_med))
census_on <- tar_read(raw_geom_data_on)
prod_on <-prod_on %>%
  filter(GeoUID == "3560004") %>%
  filter(Date < "2002")


prod_on_new_2 <- prod_on %>%
  mutate(
    mean = mapply(
      getmean_geouid,
      MoreArgs = list(
        country_raster = cmip5_med,
        census_geoms = census_on
      ),
      geouid = GeoUID,
      time = Date
    )
  )

