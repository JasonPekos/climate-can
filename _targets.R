# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Tar Quarto etc.

# Set target options:
tar_option_set(
  packages = c("tibble",
               "ggplot2",
               "dplyr",
               "zoo",
               "lubridate",
               "tidyr",
               "broom",
               "bayesplot",
               "quarto",
               "patchwork",
               "raster",
               "terra",
               "bmstdr",
               "imputeTS"), # packages that your targets need to run
  format = "rds" # default storage format
)

options(clustermq.scheduler = "multicore")

# options(
#   clustermq.scheduler = "ssh",
#   clustermq.ssh.host = "abhiroop@graham.computecanada.ca" # Change this
# )

tar_source()

list(
  #' ---
  #'Downloads ----------------------------
  #' ---
  tar_download(
    name = download_cmip5_temp,
    urls = c("https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP2.6/monthly_ens/absolute/CMIP5_rcp2.6_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc", # nolint
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP4.5/monthly_ens/absolute/CMIP5_rcp4.5_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc",
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP8.5/monthly_ens/absolute/CMIP5_rcp8.5_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc"
    ), # nolint
    paths = c("Data/CMIP5/futuretemplow.ncdf",
              "Data/CMIP5/futuretempmed.ncdf",
              "Data/CMIP5/futuretemphigh.ncdf"),
    method = "auto"
  ),
  tar_download(
    name = download_cmip5_pcp,
    urls = c("https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP2.6/monthly_ens/absolute/CMIP5_rcp2.6_monthly_abs_latlon1x1_PCP_pctl50_P1M.nc", # nolint
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP4.5/monthly_ens/absolute/CMIP5_rcp4.5_monthly_abs_latlon1x1_PCP_pctl50_P1M.nc",
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP8.5/monthly_ens/absolute/CMIP5_rcp8.5_monthly_abs_latlon1x1_PCP_pctl50_P1M.nc"
    ), # nolint
    paths = c("Data/CMIP5/futurepcplow.ncdf",
              "Data/CMIP5/futurepcpmed.ncdf",
              "Data/CMIP5/futurepcphigh.ncdf"),
    method = "auto"
  ),
  tar_download(
    name = download_cmip5_sndpt,
    urls = c("https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP2.6/monthly_ens/absolute/CMIP5_rcp2.6_monthly_abs_latlon1x1_SNDPT_pctl50_P1M.nc", # nolint
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP4.5/monthly_ens/absolute/CMIP5_rcp4.5_monthly_abs_latlon1x1_SNDPT_pctl50_P1M.nc",
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/scenarios/RCP8.5/monthly_ens/absolute/CMIP5_rcp8.5_monthly_abs_latlon1x1_SNDPT_pctl50_P1M.nc"
    ), # nolint
    paths = c("Data/CMIP5/futuresndptlow.ncdf",
              "Data/CMIP5/futuresndptmed.ncdf",
              "Data/CMIP5/futuresndpthigh.ncdf"),
    method = "auto"
  ),
  tar_download(
    name = download_hist_cmip5,
    urls = c("https://dd.weather.gc.ca/climate/cmip5/netcdf/historical/monthly_ens/absolute/CMIP5_hist_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc",
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/historical/monthly_ens/absolute/CMIP5_hist_monthly_abs_latlon1x1_PCP_pctl50_P1M.nc",
             "https://dd.weather.gc.ca/climate/cmip5/netcdf/historical/monthly_ens/absolute/CMIP5_hist_monthly_abs_latlon1x1_SNDPT_pctl50_P1M.nc"
    ), # nolint
    paths = c("Data/CMIP5/hist_temp.ncdf",
              "Data/CMIP5/hist_pcp.ncdf",
              "Data/CMIP5/hist_sndpt.ncdf"),
    method = "auto"
  ),
  #' ---
  #' READ IN THE RAW DATA ----------------------------
  #' ---
  
  #' CMPI5
  tar_target(name = raw_cmip5_hist_temp,
             command = rast(download_hist_cmip5[1]) %>%
               modify_time_labels() %>%
               terra::wrap()
  ),
  tar_target(name = raw_cmip5_hist_pcp,
             command = rast(download_hist_cmip5[2]) %>%
               modify_time_labels() %>%
               terra::wrap()
  ),
  tar_target(name = raw_cmip5_hist_sndpt,
             command = rast(download_hist_cmip5[3]) %>%
               modify_time_labels() %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_low_temp,
             command = rast(download_cmip5_temp[1]) %>% 
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_temp)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_med_temp,
             command = rast(download_cmip5_temp[2]) %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_temp)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_high_temp,
             command = rast(download_cmip5_temp[3]) %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_temp)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_low_pcp,
             command = rast(download_cmip5_pcp[1]) %>% 
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_pcp)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_high_pcp,
             command = rast(download_cmip5_pcp[3]) %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_pcp)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_low_sndpt,
             command = rast(download_cmip5_pcp[1]) %>% 
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_sndpt)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_high_sndpt,
             command = rast(download_cmip5_pcp[3]) %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist_sndpt)) %>%
               terra::wrap()
  ),
  
  #' PRODUCTIVITY
  tar_target(name = raw_prod_data_ab,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inAlberta.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_bc,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inBritish Columbia.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_mb,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inManitoba.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_nb,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNew Brunswick.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_nl,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNewfoundland and Labrador.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_ns,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNova Scotia.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_on,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inOntario.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_pe,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inPrince Edward Island.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_qc,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inQuebec.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  tar_target(name = raw_prod_data_sk,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inSaskatchewan.csv") %>% 
               mutate(Date = as.Date(Date)) %>%
               complete(GeoUID, provincename, Date = seq.Date(min(Date), as.Date("2035-01-01"), by = "month"))
  ),
  
  #' GeoJson Reads. 
  tar_target(name = raw_geom_data_on,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_ON_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_mb,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_MB_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_sk,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_SK_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_ab,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_AB_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_qc,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_QC_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_bc,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_BC_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_nu,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_NU_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_nt,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_NT_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_yt,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_YT_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_nb,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_NB_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_nl,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_NL_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_ns,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_NS_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  tar_target(name = raw_geom_data_pe,
             command = {
               sf::st_read("Data/geojson_files/1.a_census_data_PE_CSD_geometry_only.geojson") %>%
                 sf::st_transform(crs = sf::st_crs(unwrap(raw_cmip5_hist_temp)))
             }
  ),
  #' PRODUCTIVITY DATA
  #' ---
  #' put the productivity data here.
  #' ---
  tar_target(name = pe_ts,
             command = {
               command = transform_df(raw_prod_data_pe,
                                      raw_geom_data_pe,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = on_ts,
             command = {
               command = transform_df(raw_prod_data_on,
                                      raw_geom_data_on,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = ab_ts,
             command = {
               command = transform_df(raw_prod_data_ab,
                                      raw_geom_data_ab,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = nl_ts,
             command = {
               command = transform_df(raw_prod_data_nl,
                                      raw_geom_data_nl,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = mb_ts,
             command = {
               command = transform_df(raw_prod_data_mb,
                                      raw_geom_data_mb,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = bc_ts,
             command = {
               command = transform_df(raw_prod_data_bc,
                                      raw_geom_data_bc,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = sk_ts,
             command = {
               command = transform_df(raw_prod_data_sk,
                                      raw_geom_data_sk,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = qc_ts,
             command = {
               command = transform_df(raw_prod_data_qc,
                                      raw_geom_data_qc,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = nb_ts,
             command = {
               command = transform_df(raw_prod_data_nb,
                                      raw_geom_data_nb,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  tar_target(name = ns_ts,
             command = {
               command = transform_df(raw_prod_data_ns,
                                      raw_geom_data_ns,
                                      "2030-01-01",
                                      cmip5_high_temp,
                                      cmip5_low_temp,
                                      cmip5_low_pcp,
                                      cmip5_high_pcp)
             }
  ),
  #' Weight Matrices
  tar_target(name = weightmat_pe,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_pe, pe_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_on,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_on, on_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  # tar_target(name = weightmat_bc,
  #            command = {
  #              nb_results <- create_neighbors_list(raw_geom_data_bc, bc_ts)
  #              create_neighborhood_matrix(nb_results$nb)
  #            }
  # ),
  # tar_target(name = weightmat_nl,
  #            command = {
  #              nb_results <- create_neighbors_list(raw_geom_data_nl, nl_ts)
  #              create_neighborhood_matrix(nb_results$nb)
  #            }
  # ),
  tar_target(name = weightmat_ab,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_ab, ab_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_sk,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_sk, sk_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_ns,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_ns, ns_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_mb,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_mb, mb_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_qc,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_qc, qc_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_nb,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_nb, nb_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  # MODELING
  tar_target(
    name = mcmc_pars,
    command = list(
      "samples" = 5000,
      "burnin" = 1000,
      "thin" = 10
    )
   ),
  #1. PEI
  tar_target(
    name = model_fit_pe,
    command = {
      train <- pe_ts

      train <- pe_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
        

      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))

      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high

      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                        W=weightmat_pe, model="linear", family="gaussian", package="CARBayesST",
                        validrows = vs,
                        N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                       W=weightmat_pe, model="anova", family="gaussian", package="CARBayesST",
                       validrows = vs,
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                        W=weightmat_pe, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                        validrows = vs,
                        N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
               W=weightmat_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
               validrows = vs,
               N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #2. NS
  tar_target(
    name = model_fit_ns,
    command = {
      train <- ns_ts

      train <- ns_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ns, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_ns, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ns, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ns, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #3. NB
  tar_target(
    name = model_fit_nb,
    command = {
      train <- nb_ts
      
      train <- nb_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nb, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_nb, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nb, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nb, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #4. AB
  tar_target(
    name = model_fit_ab,
    command = {
      train <- ab_ts
      
      train <- ab_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ab, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_ab, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ab, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_ab, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #5. SK
  tar_target(
    name = model_fit_sk,
    command = {
      train <- sk_ts
      
      train <- sk_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_sk, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_sk, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_sk, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_sk, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #6. MB
  tar_target(
    name = model_fit_mb,
    command = {
      train <- mb_ts
      
      train <- mb_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_mb, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_mb, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_mb, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_mb, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #7. ON
  tar_target(
    name = model_fit_on,
    command = {
      train <- on_ts
      
      train <- on_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_on, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_on, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_on, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_on, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #8. QC
  tar_target(
    name = model_fit_qc,
    command = {
      train <- qc_ts
      
      train <- qc_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_qc, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_qc, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_qc, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_qc, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #9. BC
  tar_target(
    name = model_fit_bc,
    command = {
      train <- bc_ts
      
      train <- bc_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_bc, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_bc, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_bc, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_bc, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  #10. NL
  tar_target(
    name = model_fit_nl,
    command = {
      train <- nl_ts
      
      train <- nl_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #Impute missing covariates
      train[,4:7] <- apply(train[,4:7], 2, na_interpolation, option = "spline")
      
      # Validation Rows
      vs = sample(nrow(train), 0.1*nrow(train))
      
      # Formula
      form <- tot_prod ~ mean_temp_high + mean_pcp_high
      
      # Fit
      list(
        "lin" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nl, model="linear", family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "av" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                         W=weightmat_nl, model="anova", family="gaussian", package="CARBayesST",
                         validrows = vs,
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar1" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nl, model="ar", AR = 1, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin),
        
        "ar2" <- Bcartime(formula=form, data=train, scol= "s", tcol= "t",
                          W=weightmat_nl, model="ar", AR = 2, family="gaussian", package="CARBayesST",
                          validrows = vs,
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      )
    }
  ),
  # MODEL RESULTS
  # 1. PEI
  tar_target(name = results_table_pe,
             command = {
               m <- model_fit_pe
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #2. NS
  tar_target(name = results_table_ns,
             command = {
               m <- model_fit_ns
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #3. NB
  tar_target(name = results_table_nb,
             command = {
               m <- model_fit_nb
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #4. AB
  tar_target(name = results_table_ab,
             command = {
               m <- model_fit_ab
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #5. SK
  tar_target(name = results_table_sk,
             command = {
               m <- model_fit_sk
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #6. MB
  tar_target(name = results_table_mb,
             command = {
               m <- model_fit_mb
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #7. ON
  tar_target(name = results_table_on,
             command = {
               m <- model_fit_on
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #8. QC
  tar_target(name = results_table_qc,
             command = {
               m <- model_fit_qc
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #9. BC
  tar_target(name = results_table_bc,
             command = {
               m <- model_fit_bc
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  #10. NL
  tar_target(name = results_table_nl,
             command = {
               m <- model_fit_nl
               
               mc1 <- m[[1]]$mchoice
               mc2 <- m[[2]]$mchoice
               mc3 <- m[[3]]$mchoice
               mc4 <- m[[4]]$mchoice
               
               mv1 <- m[[1]]$stats
               mv2 <- m[[2]]$stats
               mv3 <- m[[3]]$stats
               mv4 <- m[[4]]$stats
               
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
               a
             }
  ),
  # Model Preds for high and low emmission
  #1. PEI
  tar_target(
    name = preds_pe, #change province here
    command = {
      test <- pe_ts #change province here
      
      #change province here
      train <- pe_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- pe_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
        high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                          W=weightmat_pe, model="ar", AR =2, family="gaussian", package="CARBayesST",
                          N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
        
        low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                         W=weightmat_pe, model="ar", AR =2, family="gaussian", package="CARBayesST",
                         N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
        
        # Get the predictions
        tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
        tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
        
        # Add the predictions with the pe_model dataframe
        pred_h <- c(train$tot_prod, tot_prod_preds_high)
        pred_l <- c(train$tot_prod, tot_prod_preds_low)
        
        #change province here
        pred <- cbind(pe_ts, pred_h, pred_l) %>%
          mutate(
            diff = pred_h - pred_l,
            rel_diff = (pred_h - pred_l)/abs(pred_l))
        pred
    }
  ),
  #2. NS
  tar_target(
    name = preds_ns,
    command = {
      test <- ns_ts
      
      train <- ns_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      test <- ns_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_ns, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_ns, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      pred <- cbind(ns_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #3. NB
  tar_target(
    name = preds_nb,
    command = {
      test <- nb_ts
      
      train <- nb_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      test <- nb_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_nb, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_nb, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      pred <- cbind(nb_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #4. AB
  tar_target(
    name = preds_ab, #change province here
    command = {
      test <- ab_ts #change province here
      
      #change province here
      train <- ab_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- ab_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_ab, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_ab, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(ab_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  # Poster stuff
  tar_target(name = mean_trends,
             command = {
               list("hist"= getmean_raster(unwrap(raw_cmip5_hist_temp), 1165:1260),
                    "high"= getmean_raster(unwrap(cmip5_high_temp), 1:289),
                    "low" = getmean_raster(unwrap(cmip5_low_temp), 1:289))
             }
  ),
  #5. SK
  tar_target(
    name = preds_sk, #change province here
    command = {
      test <- sk_ts #change province here
      
      #change province here
      train <- sk_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- sk_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_sk, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_sk, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(sk_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #6. MB
  tar_target(
    name = preds_mb, #change province here
    command = {
      test <- mb_ts #change province here
      
      #change province here
      train <- mb_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- mb_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_mb, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_mb, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(mb_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #7. ON
  tar_target(
    name = preds_on, #change province here
    command = {
      test <- on_ts #change province here
      
      #change province here
      train <- on_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- on_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_on, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_on, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(on_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #8. QC
  tar_target(
    name = preds_qc, #change province here
    command = {
      test <- qc_ts #change province here
      
      #change province here
      train <- qc_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- qc_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_qc, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_qc, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(qc_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #9. BC
  tar_target(
    name = preds_bc, #change province here
    command = {
      test <- bc_ts #change province here
      
      #change province here
      train <- bc_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- bc_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_bc, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_bc, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(bc_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #10. NL
  tar_target(
    name = preds_nl, #change province here
    command = {
      test <- nl_ts #change province here
      
      #change province here
      train <- nl_ts %>%
        filter(Date < "2006-01-01") %>%
        dplyr::select(GeoUID, Date, tot_prod, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, t, s)
      
      #change province here
      test <- nl_ts %>%
        dplyr::select(GeoUID, Date, mean_temp_high, mean_temp_low, mean_pcp_high, mean_pcp_low, tot_prod, tot_prod_na, t, s)
      
      #Impute missing covariates
      test[,3:6] <- apply(test[,3:6], 2, na_interpolation, option = "spline")
      
      # Formula
      form_high <- tot_prod_na ~ mean_temp_high + mean_pcp_high
      form_low <- tot_prod_na ~ mean_temp_low + mean_pcp_low
      
      # Fit: change weight matrices for provinces
      high <- Bcartime(formula=form_high, data=test, scol= "s", tcol= "t",
                       W=weightmat_nl, model="ar", AR =2, family="gaussian", package="CARBayesST",
                       N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      low <- Bcartime(formula=form_low, data=test, scol= "s", tcol= "t",
                      W=weightmat_nl, model="ar", AR =2, family="gaussian", package="CARBayesST",
                      N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)
      
      # Get the predictions
      tot_prod_preds_high <- apply(high$fit$samples$Y, 2, mean)
      tot_prod_preds_low <- apply(low$fit$samples$Y, 2, mean)
      
      # Add the predictions with the pe_model dataframe
      pred_h <- c(train$tot_prod, tot_prod_preds_high)
      pred_l <- c(train$tot_prod, tot_prod_preds_low)
      
      #change province here
      pred <- cbind(nl_ts, pred_h, pred_l) %>%
        mutate(
          diff = pred_h - pred_l,
          rel_diff = (pred_h - pred_l)/abs(pred_l))
      pred
    }
  ),
  #' PRODUCTIVITY PLOTS
  
  
  tar_render(name = poster,
            "Poster_file/poster.rmd"
  )
)
