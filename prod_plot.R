library(targets)
library(profvis)
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyterra)
library(patchwork)

## PEI ##
prod_data <- pe_model_pred
census <- tar_read(raw_geom_data_pe)
time_of_interest <- as.Date("2020-01-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))


prod_subset <- prod_data %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character


# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(prod_subset$mean_temp_high, na.rm = T)

# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = mean_temp_high), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Mean Temp", title = paste("Mean Temp at", time_of_interest)) +
  theme_void()


#' Average temp increase between 2005 - 2020 (plot)
#' Take diff of the climate variables between these years and plot
prod_subset_05 <- prod_data %>%
  filter(Date == as.Date("2005-01-01")) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

prod_subset_20 <- prod_data %>%
  filter(Date == as.Date("2020-01-01")) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

inc_mth_20_05 <- (prod_subset_20$mean_temp_high - prod_subset_05$mean_temp_high)/abs(prod_subset_05$mean_temp_high)

prod_subset = cbind(prod_subset, inc_mth_20_05)

# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(census_sf$inc_mth_20_05, na.rm = T)

# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = inc_mth_20_05), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Mean Temp", title = "Relative Mean Temp Increase from 2005 to 2020") +
  theme_void()



## ONTARIO ##

prod_data <- on_train
census <- tar_read(raw_geom_data_on)
time_of_interest <- as.Date("2005-12-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))


prod_subset <- prod_data %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character


# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")


lim = range(prod_subset$mean_temp_high, na.rm = T)

# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = mean_temp_high), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Mean Temp", title = paste("Mean Temp at", time_of_interest)) +
  theme_void()


## ALBERTA ##

prod_data <- tar_read(ab_ts)
census <- tar_read(raw_geom_data_ab)
time_of_interest <- as.Date("2005-12-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))


prod_subset <- prod_data %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character


# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")


lim = range(prod_subset$mean_temp_high, na.rm = T)

# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = as.numeric(mean_temp_high)), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Mean Temp", title = paste("Mean Temp at", time_of_interest)) +
  theme_void()



