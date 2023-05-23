library(spdep)

create_neighbors_list <- function(geom_data, prov_ts) {
  
  space_ids <- unique(prov_ts$s)
  
  geom_data <- geom_data[space_ids,]
  
  sf_use_s2(FALSE)
  geom_data <- as_Spatial(geom_data)
  
  nb <- poly2nb(geom_data, row.names = seq_along(geom_data$GeoUID), queen = FALSE, snap = 0.5)
  
  listw <- nb2listw(nb, style = "B")
  
  return(list(nb = nb, listw = listw))
}

create_neighborhood_matrix <- function(nb) {
  
  nb_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)
  
  return(nb_matrix)
}