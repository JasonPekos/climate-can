library(targets)
library(dplyr)

t_low <- unwrap(tar_read(raw_cmip5_future_low))
t_med <- unwrap(tar_read(raw_cmip5_future_med))
t_high <- unwrap(tar_read(raw_cmip5_future_high))


ggplot() +
  geom_smooth(aes(x = time(t_low), y = getmean_raster(t_low, 1:1140)$mean))+ 
  geom_smooth(aes(x = time(t_low), y = getmean_raster(t_med, 1:1140)$mean))+ 
  geom_smooth(aes(x = time(t_low), y = getmean_raster(t_high, 1:1140)$mean))

plot(t_diff[[time(t_diff) == as.Date("2006-01-16")]])
# merge the two tas files, keeping the time


prod_on <- tar_read(raw_prod_data_on)
cmip5_med <- unwrap(tar_read(cmip5_med))
census_on <- tar_read(raw_geom_data_on)
prod_on <- prod_on %>%
  filter(GeoUID == "3560004")


prod_on <- prod_on %>%
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
