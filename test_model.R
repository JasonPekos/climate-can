library(bmstdr)
library(raster)
library(spdep)
library(sf)
library(tidyverse)
library(lubridate)

# make the variables in appropriate form
p_pe <- tar_read(pe_ts_test)
p_pe$mean_temp <- as.numeric(p_pe$mean_temp)
p_pe$Date <- as.Date(p_pe$Date)
p_pe <- p_pe %>% 
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  mutate(t = rep(seq(1:36), 112)) %>%
  mutate(s = rep(seq(1,112), each = 36))
  

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


#Neighbours matrix
w_pe <- nb2mat(wr_pe, style='B', zero.policy = TRUE) 
dim(w_pe)

#Plot links b/w polygons of PEI
par(mai=c(0,0,0,0))
plot(raw_geom_data_pe, col='gray', border='blue')
xy <- coordinates(raw_geom_data_pe)
plot(wr_pe, xy, col='red', lwd=2, add=TRUE)

#' Modelling productivity (Y) in PEI (by each industry)
#' Base model: Bayesian GLM with Normal distribution for Y_{it}
#' Climate variables: use fixed effects
#' For spatial (S) and temporal (T) -> use random effects 
#' Model 1: `Linear` trend and NCAR for intercept and slope
#' Model 2: `Anova` with NCAR for both S and T and Normal for interaction
#' Model 3: `Temporal AR-1`
#' Model 4: `Temporal AR-2`
#' Model 5: `Adaptive CAR-AR` (NOT WORKING)
#' Model 6: ST model using INLA (NOT WORKING)


#' Set MCMC run parameters
#' Below specification produces 10,000 samples from posterior distribution
Ncar <- 50000
burn.in.car <- 10000
thin <- 10

#' How to decide which industry to model for each province?
#' 1: Check how the dominant industry for each province is impacted
#' 2. Take those industries that are dominant over the most regions

table(p_pe$Dominant_NAICS)
# 1 -> Finance; 2 -> Agriculture

vs = sample(nrow(p_pe), 0.1*nrow(p_pe)) # set the rows to use model validation

#### Industry 1 ###
## Agriculture.forestry.fishing.hunting.21.Mining.quarrying.and.oil.and.gas.extraction
####

f.pe.11 <-  production_in_division_X11.Agriculture.forestry.fishing.hunting.21.Mining.quarrying.and.oil.and.gas.extraction ~  mean_temp

## Model 1
M.lin.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "GeoUID", tcol= c("year"), 
                 W=w_pe, model="linear", family="gaussian", package="CARBayesST",
                 validrows = vs,
                 N=Ncar, burn.in=burn.in.car, thin=thin)

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice1 = M.lin.pe.11$mchoice
mvalid1 = M.lin.pe.11$stats

## Model 2
M.av.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "GeoUID", tcol="Date", 
                        W=w_pe, model="anova", family="gaussian", package="CARBayesST",
                        validrows = vs,
                        N=Ncar, burn.in=burn.in.car, thin=thin)

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice2 = M.av.pe.11$mchoice
mvalid2 = M.av.pe.11$stats

## Model 3
M.ar1.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "GeoUID", tcol="Date", 
                       W=w_pe, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                       validrows = vs,
                       N=Ncar, burn.in=burn.in.car, thin=thin)

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice3 = M.ar1.pe.11$mchoice
mvalid3 = M.ar1.pe.11$stats

## Model 4
M.ar2.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "GeoUID", tcol= "Date", 
                       W=w_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                       validrows = vs,
                       N=Ncar, burn.in=burn.in.car, thin=thin)

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice4 = M.ar2.pe.11$mchoice
mvalid4 = M.ar2.pe.11$stats

## Model 5
#' Need to remove or `impute` NA values for this model
#' GeoUID: 1101050 has all NA values
p_pe_na <- drop_na(p_pe)
vs_na = sample(nrow(p_pe_na), 0.1*nrow(p_pe_na))

which(raw_geom_data_pe$GeoUID == 1101050)
#30

w_pe_na <- w_pe[-30, -30]
dim(w_pe_na)

M.adp.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe_na, scol= "s", tcol= "t", 
                       W=w_pe_na, model="adaptive", family="gaussian", package="CARBayesST",
                       validrows = vs_na,
                       N=Ncar, burn.in=burn.in.car, thin=thin)

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice5 = M.adp.pe.11$mchoice

## Model 6
M.inla.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "s", tcol= "t", 
                       W=w_pe, model= c("bym" ,"ar1"), family="gaussian", package="inla")

#'Extract the summaries needed!
#' 1: Model choice criteria
#' 2. Model validation statistics
mchoice6 = M.inla.pe.11$mchoice


#' Model comparison
a <- rbind(mchoice1, mchoice2, mchoice3, mchoice4)
a <- a[, -(5:6)]
a <- a[, c(2, 1, 4, 3)]
b.rmse <- c(mvalid1$rmse, mvalid2$rmse, mvalid3$rmse, mvalid4$rmse)
a <- cbind(a, b.rmse)
rownames(a) <- c("Linear", "ANOVA",  "AR(1)", "AR(2)")
colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC", "RMSE") 
table.pei.11 <- a


#' out of the 4 models, AR(2) is the best
#' 1. Get parameter estimates of the best model
summary(M.ar2.pe.11)


#### Industry 2 ###
## Finance.and.insurance.53.Real.estate.and.rental.and.leasing
####

f_pe_53 <- production_in_division_X52.Finance.and.insurance.53.Real.estate.and.rental.and.leasing ~ mean_temp

M_pe_11 <- Bcartime(formula=f_pe_53, data=p_pe, scol= "GeoUID", tcol="Date", 
                    W=w_pe, model="ar", family="gaussian", package="CARBayesST",
                    validrows = vs,
                    N=Ncar, burn.in=burn.in.car, thin=thin)
