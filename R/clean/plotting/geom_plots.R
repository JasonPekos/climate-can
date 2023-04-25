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

geom_region_plot <- function(tidyclimate, tidygeo, region_names) {
    #' JCP: Plot a region on a map, with weather stations.
    #' tidyclimate: A tidy climate dataframe
    #' tidygeo: A tidy geo dataframe
    #' region_names: A vector of region names to plot.
    #' NOT weather stations, but regions.

    points <- tidyclimate %>% # Filter out the regions we want.
        filter(Region.Name %in% region_names) #nolint

    geos <- tidygeo %>% # Filter out the regions we want.
        filter(Region.Name %in% region_names) #nolint

    p <- ggplot() +
        geom_polygon(data = geos,
                   aes(x = long, y = lat, group = group), #nolint
                   fill = "#69b3a2",
                   color = "white") +
        geom_point(data = points, aes(x = Longitude..x., y = Latitude..y.)) #nolint

    return(p)
}