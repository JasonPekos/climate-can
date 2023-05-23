library(targets)
library(profvis)
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyterra)
library(patchwork)



prod <- tar_read(on_ts)
census <- tar_read(raw_geom_data_on)
time_of_interest <- as.Date("1999-01-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))


filtered_rast <- rastc %>%
  mask(census) %>%
  crop(census) 

prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID),  # Convert GeoUID to character
         mean_temp_high = map_dbl(mean_temp_high, ~ .x[[1]])) # Convert list to float


# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

# Get range
# a <- max(prod_subset$mean_temp_high, na.rm = TRUE)
# b <- min(prod_subset$mean_temp_high, na.rm = TRUE)
# 
# ra <- range(a,b)

# Plot
preparsed <- ggplot() +
  geom_spatraster(data = filtered_rast[[1]]) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  geom_sf(data = census_sf, fill = "NA", color = "black") +
  ggtitle("Raw CMIP5 Predictions") +
  theme_void() + 
  theme(legend.position = "none")  



parsed <- ggplot() +
  geom_sf(data = census_sf, aes(fill = mean_temp_high), color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(fill = "Mean Temp") +
  ggtitle("CMIP5 Predictions Averaged Over \n Census Regions") +
  theme_void() + 
  theme(legend.position = "none")   

preparsed + parsed
