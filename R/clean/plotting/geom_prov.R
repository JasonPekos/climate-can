plot_geom <- function(tidygeom, color = "white", fill = "#B97C7C") {
  p <- ggplot() +
    geom_polygon(data = tidygeom,
                 aes(x = long, y = lat, group = group),
                 fill = fill,
                 color = color) +
    coord_map() +
    theme_void()

  return(p)
}