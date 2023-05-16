library(targets)
library(parallel)
library(profvis)




prod <- tar_read(nl_ts_test)
census <- st_as_sf(tar_read(raw_geom_data_nl))
time_of_interest <- as.Date("1999-01-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_low))



prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID),  # Convert GeoUID to character
         mean_temp = map_dbl(mean_temp, ~ .x[[1]]))


# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = mean_temp)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(fill = "Mean Temp", title = paste("Mean temperatures at", time_of_interest))

