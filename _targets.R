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
               "bmstdr"), # packages that your targets need to run
  format = "rds" # default storage format
)

options(clustermq.scheduler = "multicore")

# options(
#   clustermq.scheduler = "ssh",
#   clustermq.ssh.host = "pekos@graham.computecanada.ca" # Change this
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
  tar_target(name = weightmat_pe,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_pe, pe_ts)
               create_neighborhood_matrix(nb_results$nb)
             }
  ),
  tar_target(name = weightmat_on,
             command = {
               nb_results <- create_neighbors_list(raw_geom_data_on, on_ts)
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
  #              nb_results <- create_neighbors_list(raw_geom_data_nl)
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
  # MODELING
  tar_target(
    name = mcmc_pars,
    command = list(
      "samples" = 5000,
      "burnin" = 1000,
      "thin" = 10
    )
   ),
  tar_target(
    name = ar2_train_pe,
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
      Bcartime(formula=form, data=train, scol= "s", tcol= "t",
               W=weightmat_pe, model="ar", AR = 2, family="gaussian", package="CARBayesST",
               validrows = vs,
               N=mcmc_pars$samples, burn.in=mcmc_pars$burnin, thin=mcmc_pars$thin)

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
  
  tar_render(name = poster,
             "Poster_file/poster.rmd"
  )
)
