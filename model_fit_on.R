library(sf)
library(sp)
library(spdep)
library(purrr)
library(ggplot2)
library(dplyr)
library(CARBayesST)
## Ontario Modelling##
## Read the data for ON
on_ts <- tar_read(on_ts)

# make the variables in the data in appropriate form
on_ts$mean_temp_high <- as.numeric(on_ts$mean_temp_high)
on_ts$mean_temp_low <- as.numeric(on_ts$mean_temp_low)
on_ts$mean_pcp_high <- as.numeric(on_ts$mean_pcp_high)
on_ts$mean_pcp_low <- as.numeric(on_ts$mean_pcp_low)

ns_on = length(unique(on_ts$GeoUID)) #No of regions
nt_on = length(unique(on_ts$Date)) #No of time points

#' Get the training data for fitting model
on_train <- on_ts %>%
  filter(Date < "2006-01-01") %>%
  dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s, tot_prod)

#' Which regions don't have missing value for tot_prod??
reg_on_naprod <- unique(on_ts$GeoUID[!is.na(on_ts$tot_prod)])
s_on_naprod <- unique(on_ts$s[!is.na(on_ts$tot_prod)])

#' Keep regions from on_train
#on_train <- on_train %>%
#  filter(GeoUID %in% reg_on_naprod)

#' Make the dataframe corresponding to high emission
#' with NA values for tot_prod for years post 2006
#' --> This is needed for predictions
on_model <- on_ts %>%
  filter(GeoUID %in% reg_on_naprod) %>%
  dplyr::select(GeoUID, Date, mean_temp_high, mean_pcp_high, mean_temp_low, mean_pcp_low, tot_prod, t, s) %>%
  mutate(tot_prod_na = if_else(Date >= "2006-01-01", NA, tot_prod))


#' Constructing Neigbourhood matrix for ON
#' 1. Load Polygons Data Frame (type "sf")
#' 2. Convert "sf" to Spatialpolydf
#' 3. Convert polygons to the neighbour's list (poly2nb)
#' (either using "rook's case" or "queen")
#' 4. Convert neighbour's list to neighbourhood matrix (nb2mat)
#' 5. Plot the links b/w regions using neighbor's list

raw_geom_data_on <- as_Spatial(tar_read(raw_geom_data_on))

#switch off spherical geometry
sf_use_s2(FALSE)

## For Rook's case
#CreatNeigbhour's list
nr_on <- create_neighbors_list(tar_read(raw_geom_data_on))
#neighbour matrix
wr_on <- create_neighborhood_matrix(nr_on[[1]])
dim(wr_on)

#Plot links b/w polygons of PEI
par(mai=c(0,0,0,0))
plot(raw_geom_data_on, col='gray', border='blue')
xy <- coordinates(raw_geom_data_on)
plot(nr_on[[2]], xy, col='red', lwd=2, add=TRUE)

#' Estimate weight matrix
#' 1. make on_sp
on_sp = split(on_ts, factor(on_ts$s))
#' Avg prod over all regions 
Y_on = numeric()
for(i in 1:length(on_sp)){
  Y_on[i] = mean(on_sp[[i]]$tot_prod, na.rm = T)
}
Y_on_wo_na = na.omit(Y_on)
wr_on <- wr_on[s_on_naprod, s_on_naprod]
w_est_on <- W.estimate(wr_on, Y_on_wo_na) #Need CARBayesST package

#' Set MCMC run parameters
#' Below specification produces 3,000 samples from posterior distribution
Ncar <- 40000
burn.in.car <- 10000
thin <- 10

# set the rows to use model validation
vs = sample(nrow(on_train), 0.1*nrow(on_train))

# Specify the linear model
f.on <- tot_prod ~ mean_temp_high + mean_pcp_high


#########################
## For model selection ##
#########################
## Model 1
M.lin.on <- Bcartime(formula=f.on, data=on_train, scol= "s", tcol= "t",
                     W=w_est_on, model="linear", family="gaussian", package="CARBayesST",
                     validrows = vs,
                     N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice1 = M.lin.pe$mchoice
mvalid1 = M.lin.pe$stats

## Model 2
M.av.on <- Bcartime(formula=f.on, data=on_train, scol= "s", tcol= "t",
                    W=w_on, model="anova", family="gaussian", package="CARBayesST",
                    validrows = vs,
                    N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice2 = M.av.pe$mchoice
mvalid2 = M.av.pe$stats


## Model 4
M.ar2.on <- Bcartime(formula=f.on, data=on_train, scol= "s", tcol= "t",
                     W=wr_on, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                     validrows = vs,
                     N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice4 = M.ar2.pe$mchoice
mvalid4 = M.ar2.pe$stats