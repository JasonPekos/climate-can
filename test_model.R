library(bmstdr)
library(raster)
library(spdep)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(INLAspacetime)

# make the variables in the data in appropriate form
p_pe <- tar_read(pe_ts_test)
p_pe$mean_temp <- as.numeric(p_pe$mean_temp)
p_pe$Date <- as.Date(p_pe$Date)

ns = length(unique(p_pe$GeoUID)) #No of regions
nt = length(unique(p_pe$Date)) #No of time points
  
p_pe <- p_pe %>% 
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  mutate(t = rep(seq(1:nt), ns)) %>%
  mutate(s = rep(seq(1,ns), each = nt))

#overall productiivity
ovl_prod <- p_pe %>%
  select(starts_with("production")) %>%
  apply(1, mean)

p_pe = cbind(p_pe, ovl_prod)

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

#Plot links b/w polygons of PEI
par(mai=c(0,0,0,0))
plot(raw_geom_data_pe, col='gray', border='blue')
xy <- coordinates(raw_geom_data_pe)
plot(wr_pe, xy, col='red', lwd=2, add=TRUE)

library(ggplot2)
library(bmstdr)
p_pe_1 <- pe_ts_full %>%
  filter(Date == "2001-01-01")
p_pe_1 <- cbind(p_pe_1, xy)

ggplot2::ggplot(p_pe_1, aes(x=1, y=2, fill=tot_prod)) +
  scale_fill_gradientn(colours=colpalette, na.value="black") +
  geom_polygon(colour='black',size=0.25) +
  geom_polygon(data = p_pe_1, aes(x=1, y=2, fill=tot_prod), colour='black',size=0.6) +
  coord_equal()

#' Modelling productivity (Y) in PEI (by each industry)
#' Base model: Bayesian GLM with Normal distribution for Y_{it}
#' Climate variables: use fixed effects
#' For spatial (S) and temporal (T) -> use random effects 
#' Model 0: GLM without any random effects of S and T
#' Model 1: `Linear` trend and NCAR for intercept and slope
#' Model 2: `Anova` with NCAR for both S and T and Normal for interaction
#' Model 3: `Temporal AR-1`
#' Model 4: `Temporal AR-2`
#' Model 5: `Adaptive CAR-AR` (NOT WORKING)
#' Model 6: ST model using INLA (NOT WORKING)


#' Set MCMC run parameters
#' Below specification produces 3,000 samples from posterior distribution
Ncar <- 40000
burn.in.car <- 10000
thin <- 10

#' How to decide which industry to model for each province?
#' 1: Check how the dominant industry for each province is impacted
#' 2. Take those industries that are dominant over the most regions

table(p_pe$Dominant_NAICS)
names(sort(table(p_pe$Dominant_NAICS), decreasing = T)[1:2])
# 1 -> Finance; 2 -> Agriculture

vs = sample(nrow(p_pe), 0.1*nrow(p_pe)) # set the rows to use model validation

#### Industry 1 ###
## Agriculture.forestry.fishing.hunting.21.Mining.quarrying.and.oil.and.gas.extraction
####

f.pe.11 <-  production_in_division_X11.Agriculture.forestry.fishing.hunting.21.Mining.quarrying.and.oil.and.gas.extraction ~  mean_temp


#' Model 0
M0.pe.11 <- glm(formula = f.pe.11, family = "gaussian", data = p_pe)
resid.M0.pe.11 <- residuals(M0.pe.11)
summary(M0.pe.11)$coefficients
summary(M0.pe.11)$dispersion

#' Checking for spatial dependence
#' Using Moran's I statistic
moran.mc(x = resid.M0.pe.11[1:112], listw = w.list, nsim = 10000)

# Plot of Moran's statistic
#rwm <- mat2listw(w_pe, style='W')
# Checking if rows add up to 1
#mat <- listw2mat(rwm)
#apply(mat, 1, sum)[1:112]
#moran.plot(y, rwm)


## Model 1
M.lin.pe.11 <- Bcartime(formula=f.pe.11, data=p_pe, scol= "GeoUID", tcol= "Date", 
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


#### Industry ###
## Finance.and.insurance.53.Real.estate.and.rental.and.leasing
####

f_pe_tot <- ovl_prod ~ mean_temp

M_pe_tot <- Bcartime(formula=f_pe_tot, data=p_pe, scol= "GeoUID", tcol="Date", 
                    W=w_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                    validrows = vs,
                    N=Ncar, burn.in=burn.in.car, thin=thin)

## Use M_pe_tot to get the predictions

# Compute historical trend over all cells:

means <- tar_read(mean_trends) 

i1 <- length(means$hist$mean)
i2 <- length(means$high$mean)

# Plot
p1 <- ggplot() +
  geom_line(aes(x = 1:i1, y = means$hist$mean), color = "blue") + 
  geom_line(aes(x = (i1+1):(i1+i2), y = means$low$mean), color = "green", linetype = "twodash") + 
  geom_line(aes(x = (i1+1):(i1+i2), y = means$high$mean), color = "red", linetype = "twodash") + 
  theme_bw() + 
  xlab("Days Since 1999") +
  ylab("Average Temperature — Canada") + 
  ggtitle("Relationship Between Different Climate Scenarios")


p2 <- ggplot() +
  geom_line(aes(x = (i1+1):(i1+i2), y = (means$high$mean - means$low$mean)), color = "green", linetype = "twodash") + 
  theme_bw() + 
  xlab("Days Since 1999") +
  ylab("Differenced Avg — Scenarios") + 
  ggtitle("Different Climate Scenarios Predict Different Outcomes")


p1 / p2



