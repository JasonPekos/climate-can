library(bmstdr)
library(raster)
library(spdep)
library(sf)

p_pe <- tar_read(pe_ts_test)
p_pe$mean_temp <- as.numeric(p_pe$mean_temp)
p_pe$Date <- as.Date(p_pe$Date)


## Constructing Neigbourhood matrix for PEI
#Polygons Data Frame
tar_load(raw_geom_data_pe)

raw_geom_data_pe <- as_Spatial(raw_geom_data_pe)

#switch off spherical geometry
sf_use_s2(FALSE)

#Neigbhour's list
wr_pe <- poly2nb(raw_geom_data_pe, row.names=seq(1:length(raw_geom_data_pe$GeoUID)), queen=FALSE) 
wr_pe


#Neighbours matrix
w_pe <- nb2mat(wr_pe, style='B', zero.policy = TRUE) 
dim(w_pe)


#Plot links b/w polygons of PEI
par(mai=c(0,0,0,0))
plot(raw_geom_data_pe, col='gray', border='blue')
xy <- coordinates(raw_geom_data_pe)
plot(wr_pe, xy, col='red', lwd=2, add=TRUE)

## GeoUID = 112


Ncar <- 50000
burn.in.car <- 10000
thin <- 10

f_pe <-  production_in_division_X11.Agriculture.forestry.fishing.hunting.21.Mining.quarrying.and.oil.and.gas.extraction ~  mean_temp

M_pe <- Bcartime(formula=f_pe, data=p_pe, scol= "GeoUID", tcol="Date", 
                 W=w_pe, model="ar", family="gaussian", package="CARBayesST", 
                 N=Ncar, burn.in=burn.in.car, thin=thin)


M_pe <- Bcartime(formula=f_pe, data=p_pe, scol= "GeoUID", tcol="Date", 
                 W=w_pe, model="ar", family="gaussian", package="CARBayesST", 
                 N=Ncar, burn.in=burn.in.car, thin=thin)


