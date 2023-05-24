## PEI diff plot
library(targets)
library(profvis)
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(sf)
## Monthly
prod <- tar_read(preds_pe)
census <- tar_read(raw_geom_data_pe)
time_of_interest <- as.Date("2024-05-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))

prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(prod_subset$diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Diff", title = paste("Diff b/w high and low", time_of_interest)) +
  theme_void()

lim = range(census_sf$rel_diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = rel_diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Relatve diff", title = paste("Diff b/w high and low relative to low", time_of_interest)) +
  theme_void()

## Aggregated productivity from 2022 to 2030


## NS diff plot
## Monthly
prod <- tar_read(preds_ns)
census <- tar_read(raw_geom_data_ns)
time_of_interest <- as.Date("2024-05-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))

prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(prod_subset$diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Diff", title = paste("Diff b/w high and low", time_of_interest)) +
  theme_void()

lim = range(census_sf$rel_diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = rel_diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Relatve diff", title = paste("Diff b/w high and low relative to low", time_of_interest)) +
  theme_void()

## NB diff plot
## Monthly
prod <- tar_read(preds_nb)
census <- tar_read(raw_geom_data_nb)
time_of_interest <- as.Date("2024-05-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))

prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(prod_subset$diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Diff", title = paste("Diff b/w high and low", time_of_interest)) +
  theme_void()

lim = range(census_sf$rel_diff, na.rm = T)
# Plot
ggplot() +
  geom_sf(data = census_sf, aes(fill = rel_diff), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Relatve diff", title = paste("Diff b/w high and low relative to low", time_of_interest)) +
  theme_void()
