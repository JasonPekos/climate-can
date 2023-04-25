library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(geojsonio)
library(broom)
library(sf)
library(sp)
library(purrr)
sf_use_s2(FALSE)

tidy_climate <- function(raw_climate_data){
    #' JCP: Tidy the climate data
    raw_climate_data %>%
        mutate(Date.Time = as.yearmon(Date.Time, format = "%Y-%m")) %>% #nolint
        mutate(year = year(Date.Time)) %>%
        mutate(Province = NA) %>% # Add these for later use
        mutate(Region.Name = NA)

    return(raw_climate_data)
}

tidy_geom <- function(geojson_file){
    #' JCP: Tidy a geojson file in the following way:
    #' 1. Convert to tidy data format
    #' 2. Add a column with the region name,
    #' province name, and GeoUID

    tidy_dat <- tidy(geojson_file) %>% # Change to tidy data format
        mutate(id = as.integer(id)) # Change id to integer

    num_regions <- length(geojson_file$provincename) # For left_join

    properties <- as.data.frame(geojson_file) %>% # Extract properties
        select(GeoUID, # nolint
               Region.Name, # nolint
               provincename) %>% # nolint
        mutate(GeoUID = as.character(GeoUID)) %>%
        mutate(id = seq(0, num_regions - 1))

    out <- left_join(tidy_dat, properties, by = "id") # Create final dataframe.

    return(out)
}


find_enclosing_polygon <- function(lat, long, sp_object) {
    #' JCP: Find the name of the region that contains a given point.
    #' Takes a latitude, longitude, and a SpatialPolygonsDataFrame object 
    #' as input. Returns the name of the region that contains the point.
    #' ---
    #' This function is computationally expensive, because intersection
    #' search kinda sucks.

  # Convert the SpatialPolygonsDataFrame object to an sf object
  sfpoly <- st_make_valid(st_as_sf(sp_object))

  # Create a point and set CRS to match sfpoly
  point <- st_point(c(long, lat)) %>%
    st_sfc(crs = st_crs(sfpoly))

  intersecting_indices <- st_intersects(sfpoly, point)

  region_name <- sfpoly[unlist(intersecting_indices), ]$Region.Name

  # Return the associated Region.Name
  return(region_name)
}

add_enclosing_polydat <- function(tidyclimate, sp_object){
    #' JCP: Update the column (Region.Name) in the climate data with the
    #' name of the enclosing polygon (defaults to NA).
    #' ---
    #' The climate data (tidyclimate) is a tibble with
    #' columns for latitude (Latitude..y.) and longitude (Longitude..x.)
    #' ---
    #' The sp_object is a SpatialPolygonsDataFrame object.

    updated <- tidyclimate %>%
        mutate(Region.Name = map2(Latitude..y., Longitude..x., #nolint
                                  find_enclosing_polygon,
                                  sp_object = sp_object) %>%
                  purrr::map_chr(function(x) ifelse(length(x) == 0, NA, x)))

    return(updated)
}
