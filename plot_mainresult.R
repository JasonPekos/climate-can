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
## MONTHLY

#' Fucntion to make diff plots
#' Inputs: preds_prov, geojson filename, Date (as character with YYYY-MM-01)
#' Output: Plot diff of prod for given month, year
diff_plot <- function(preds_prov, raw_geom_data_prov, Date){
  
  prod <- tar_read(preds_prov)
  census <- tar_read(geom_data_prov)
  time_of_interest <- as.Date(Date)  # replace with your desired date
  rastc <- unwrap(tar_read(cmip5_high_temp))
  
  prod_subset <- prod %>%
    filter(Date == time_of_interest) %>%
    mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character
  
  # Join the spatial data with the temperature data
  census_sf <- left_join(census, prod_subset, by = "GeoUID")
  
  lim = range(prod_subset$diff, na.rm = T)
  # Plot
  p = ggplot() +
    geom_sf(data = census_sf, aes(fill = diff), color ="black") +
    scale_fill_gradient(low = "blue", high = "red", limits = lim) +
    theme_minimal() +
    labs(fill = "Diff", title = paste("Diff b/w high and low", time_of_interest)) +
    theme_void()
  
  return(p)
}

## YEARLY

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
p1 = ggplot() +
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
p2 = ggplot() +
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
p3 = ggplot() +
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


## ON diff plot
## Monthly
prod <- tar_read(preds_on)
census <- tar_read(raw_geom_data_on)
time_of_interest <- as.Date("2024-05-01")  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))

prod_subset <- prod %>%
  filter(Date == time_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) # Convert GeoUID to character

# Join the spatial data with the temperature data
census_sf <- left_join(census, prod_subset, by = "GeoUID")

lim = range(prod_subset$diff, na.rm = T)
# Plot
p4 = ggplot() +
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



