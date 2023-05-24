## PEI ##
m_pe <- tar_read(model_fit_pe)

mc1 <- m_pe[[1]]$mchoice
mc2 <- m_pe[[2]]$mchoice
mc3 <- m_pe[[3]]$mchoice
mc4 <- m_pe[[4]]$mchoice

mv1 <- m_pe[[1]]$stats
mv2 <- m_pe[[2]]$stats
mv3 <- m_pe[[3]]$stats
mv4 <- m_pe[[4]]$stats

#' Model comparison table
a <- rbind(mc1, mc2, mc3, mc4)
a <- a[, -c(2,4,5,6)]
b.rmse <- c(mv1$rmse, mv2$rmse, mv3$rmse, mv4$rmse)
b.mae <- c(mv1$mae, mv2$mae, mv3$mae, mv4$mae)
b.crps <- c(mv1$crps, mv2$crps, mv3$crps, mv4$crps)
b.cvg <- c(mv1$cvg, mv2$cvg, mv3$cvg, mv4$cvg)
a <- cbind(a, b.rmse, b.mae, b.crps, b.cvg)
rownames(a) <- c("Linear", "ANOVA",  "AR(1)", "AR(2)")
colnames(a) <- c("DIC", "WAIC", "RMSE", "MAE", "CRPS", "CVG") 
table.pei <- a


library(imputeTS)

# NS fitting
# Formula
form <- tot_prod ~ mean_temp_high + mean_pcp_high

ns_ts <- tar_read(ns_ts)
ns_ts[,22:25] <- apply(ns_ts[,22:25], 2, na_interpolation, option = "spline") #Impute missing covariates

train_ns <- ns_ts %>%
  filter(Date < "2006-01-01") %>%
  dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)

wr_ns <- tar_read(weightmat_ns)

# Validation Rows
vs = sample(nrow(train_ns), 0.1*nrow(train_ns))

Bcartime(formula=form, data=train_ns, scol= "s", tcol= "t",
         W=wr_ns, model="linear", family="gaussian", package="CARBayesST",
         validrows = vs,
         N=5000, burn.in=1000, thin=10)

Bcartime(formula=form, data=train_ns, scol= "s", tcol= "t",
         W=wr_ns, model="ar", AR=2, family="gaussian", package="CARBayesST",
         validrows = vs,
         N=5000, burn.in=1000, thin=10)


#NB fitting

# Formula
form <- tot_prod ~ mean_temp_high + mean_pcp_high

nb_ts <- tar_read(nb_ts)
nb_ts[,22:25] <- apply(nb_ts[,22:25], 2, na_interpolation, option = "spline") #Impute missing covariates

train_nb <- nb_ts %>%
  filter(Date < "2006-01-01") %>%
  dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)

wr_nb <- tar_read(weightmat_nb)

# Validation Rows
vs = sample(nrow(train_nb), 0.1*nrow(train_nb))

Bcartime(formula=form, data=train_nb, scol= "s", tcol= "t",
         W=wr_nb, model="ar", AR=2, family="gaussian", package="CARBayesST",
         validrows = vs,
         N=5000, burn.in=1000, thin=10)


