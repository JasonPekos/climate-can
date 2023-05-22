#' Model fitting strategy
#' Fit model using data from 1997-2006
#' Predict on data till 2030
#' Pacakges required
library(bmstdr)
library(raster)
library(spdep)
library(sf)
library(tidyr)
library(dplyr)

# make the variables in the data in appropriate form
pe_ts$mean_temp_high <- as.numeric(pe_ts$mean_temp_high)
pe_ts$mean_temp_low <- as.numeric(pe_ts$mean_temp_low)
pe_ts$mean_pcp_high <- as.numeric(pe_ts$mean_pcp_high)
pe_ts$mean_pcp_low <- as.numeric(pe_ts$mean_pcp_low)

#' Add total productivity column
tot_prod <- pe_ts %>%
  select(starts_with("production")) %>%
  apply(1, mean)

#' Full dataframe
pe_ts_full <-  cbind(pe_ts, tot_prod)

ns = length(unique(pe_ts_full$GeoUID)) #No of regions
nt = length(unique(pe_ts_full$Date)) #No of time points

pe_ts_full <- pe_ts_full %>%
  mutate(t = rep(seq(1:nt), ns)) %>%
  mutate(s = rep(seq(1,ns), each = nt))

#' Get the training data for fitting model
pe_train <- pe_ts_full %>%
  filter(Date < "2006-01-01") %>%
  select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)

#' Which region has missing value for tot_prod??
unique(pe_train$GeoUID[is.na(pe_train$tot_prod)])

#' remove that region from pe_train
pe_train <- pe_train %>%
  filter(GeoUID != unique(pe_train$GeoUID[is.na(pe_train$tot_prod)]))

#' Make the dataframe corresponding to high emission
#' with NA values for tot_prod for years post 2006
#' --> This is needed for predictions
pe_model <- pe_ts_full %>%
  filter(Date < "2030-01-01") %>%
  select(GeoUID, Date, tot_prod, mean_temp_high, mean_pcp_high, mean_temp_low, mean_pcp_low, t, s) %>%
  mutate(tot_prod_na = if_else(Date >= "2006-01-01", NA, tot_prod))

pe_model <- pe_model %>%
  filter(GeoUID != 1101050)


#' Constructing Neigbourhood matrix for PEI
#' 1. Load Polygons Data Frame (type "sf")
#' 2. Convert "sf" to Spatialpolydf
#' 3. Convert polygons to the neighbour's list (poly2nb)
#' (either using "rook's case" or "queen")
#' 4. Convert neighbour's list to neighbourhood matrix (nb2mat)
#' 5. Plot the links b/w regions

tar_load(raw_geom_data_pe)
raw_geom_data_pe <- as_Spatial(raw_geom_data_pe)

#switch off spherical geometry
sf_use_s2(FALSE)

## For Rook's case
#CreatNeigbhour's list
wr_pe <- poly2nb(raw_geom_data_pe, row.names=seq(1:length(raw_geom_data_pe$GeoUID)), queen=FALSE) 
wr_pe
w.list <- nb2listw(wr_pe, style = "B")

#Neighbours matrix
w_pe <- nb2mat(wr_pe, style='B', zero.policy = TRUE) 
dim(w_pe)

#' Set MCMC run parameters
#' Below specification produces 3,000 samples from posterior distribution
Ncar <- 40000
burn.in.car <- 10000
thin <- 10

# set the rows to use model validation
vs = sample(nrow(pe_train), 0.1*nrow(pe_train))

# Specify the linear model
f.pe <- tot_prod ~ mean_temp_high + mean_pcp_high

#########################
## For model selection ##
#########################
## Model 1
M.lin.pe <- Bcartime(formula=f.pe, data=pe_train, scol= "s", tcol= "t",
                     W=w_pe, model="linear", family="gaussian", package="CARBayesST",
                     validrows = vs,
                     N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice1 = M.lin.pe$mchoice
mvalid1 = M.lin.pe$stats

## Model 2
M.av.pe <- Bcartime(formula=f.pe, data=pe_train, scol= "s", tcol= "t",
                     W=w_pe, model="anova", family="gaussian", package="CARBayesST",
                     validrows = vs,
                     N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice2 = M.av.pe$mchoice
mvalid2 = M.av.pe$stats

## Model 3
M.ar1.pe <- Bcartime(formula=f.pe, data=pe_train, scol= "s", tcol= "t",
                     W=w_pe, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                     validrows = vs,
                     N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice3 = M.ar1.pe$mchoice
mvalid3 = M.ar1.pe$stats


## Model 4
M.ar2.pe <- Bcartime(formula=f.pe, data=pe_train, scol= "s", tcol= "t",
                        W=w_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                        validrows = vs,
                        N=Ncar, burn.in=burn.in.car, thin=thin)
#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice4 = M.ar2.pe$mchoice
mvalid4 = M.ar2.pe$stats

#' Model comparison table
a <- rbind(mchoice1, mchoice2, mchoice3, mchoice4)
a <- a[, -(5:6)]
a <- a[, c(2, 1, 4, 3)]
b.rmse <- c(mvalid1$rmse, mvalid2$rmse, mvalid3$rmse, mvalid4$rmse)
a <- cbind(a, b.rmse)
rownames(a) <- c("Linear", "ANOVA",  "AR(1)", "AR(2)")
colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC", "RMSE") 
table.pei <- a

###########################
## For Model Predictions ##
###########################
f.pe_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high

## Final model with predictions for high emission scenario
M.final.pe_high <- Bcartime(formula=f.pe_high, data=pe_model, scol= "s", tcol= "t",
                     W=w_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                     N=Ncar, burn.in=burn.in.car, thin=thin)


#' Final model with predictions for low emission scenario
#' tot_prod_na is the productivity till 2005
#' tot_prod is th productivity till 2021
f.pe_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low

M.final.pe_low <- Bcartime(formula=f.pe_low, data= pe_model, scol= "s", tcol= "t",
                       W=w_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                       N=Ncar, burn.in=burn.in.car, thin=thin)

# Get the predictions
# plot the predictions
library(geojsonio)

geom_pe <- generics::tidy(raw_geom_data_pe) %>% # Change to tidy data format
  mutate(id = as.integer(id))
data_pe <- as.data.frame(raw_geom_data_pe)


obs <- pe_ts_full %>% 
  filter(Date >= "2006-01-01" & Date < "2035-01-01") %>%
  select(tot_prod)




