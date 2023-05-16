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
               "geojsonio",
               "broom",
               "bayesplot",
               "quarto",
               "patchwork"), # packages that your targets need to run
  format = "rds" # default storage format
)

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
    name = download_hist,
    urls = c("https://dd.weather.gc.ca/climate/cmip5/netcdf/historical/monthly_ens/absolute/CMIP5_hist_monthly_abs_latlon1x1_TEMP_pctl50_P1M.nc"), # nolint
    paths = c("Data/CMIP5/hist.ncdf"),
    method = "auto"
  ),
  #' ---
  #' READ IN THE RAW DATA ----------------------------
  #' ---
   
  #' CMPI5
  tar_target(name = raw_cmip5_hist,
             command = rast("Data/CMIP5/hist.ncdf") %>%
               modify_time_labels() %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_low,
             command = rast("Data/CMIP5/futuretemplow.ncdf") %>% 
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_med,
             command = rast("Data/CMIP5/futuretempmed.ncdf") %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist)) %>%
               terra::wrap()
  ),
  tar_target(name = cmip5_high,
             command = rast("Data/CMIP5/futuretemphigh.ncdf") %>%
               modify_time_labels() %>%
               c(unwrap(raw_cmip5_hist)) %>%
               terra::wrap()
  ),

  #' PRODUCTIVITY
  tar_target(name = raw_prod_data_ab,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inAlberta.csv")
  ),
  tar_target(name = raw_prod_data_bc,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inBritish Columbia.csv")
  ),
  tar_target(name = raw_prod_data_mb,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inManitoba.csv")
  ),
  tar_target(name = raw_prod_data_nb,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNew Brunswick.csv")
  ),
  tar_target(name = raw_prod_data_nl,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNewfoundland and Labrador.csv")
  ),
  tar_target(name = raw_prod_data_ns,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inNova Scotia.csv")
  ),
  tar_target(name = raw_prod_data_on,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inOntario.csv")
  ),
  tar_target(name = raw_prod_data_pe,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inPrince Edward Island.csv")
  ),
  tar_target(name = raw_prod_data_qc,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inQuebec.csv")
  ),
  tar_target(name = raw_prod_data_sk,
             command = read.csv("Data/Productivity per NAICS within region/4.c_production_in_CSD_inSaskatchewan.csv")
  ),
  
  #' CLIMATE — WEATHER STATION DATA
  tar_target(name = raw_station_data,
    command = read.csv("Data/climate_data/weather_Station_data.csv")
  ),

  #' GEOMS: CENSUS AREA
  tar_target(name = raw_geom_data_on,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_ON_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_mb,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_MB_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_sk,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_SK_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_ab,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_AB_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_qc,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_QC_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_bc,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_BC_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_nu,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_NU_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_nt,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_NT_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_yt,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_YT_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_nb,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_NB_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_nl,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_NL_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_ns,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_NS_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  tar_target(name = raw_geom_data_pe,
    command = {
       geojson_read("Data/geojson_files/1.a_census_data_PE_CSD_geometry_only.geojson", # nolint
                    what = "sp")
    }
  ),
  #' PRODUCTIVITY DATA
  #' ---
  #' put the productivity data here.
  #' ---
  tar_target(name = pe_ts_test,
    command = {
      raw_prod_data_pe %>%
        filter(Date < "2002") %>%
        mutate(
          mean_temp = mapply(
            getmean_geouid,
            MoreArgs = list(
              country_raster = unwrap(cmip5_med),
              census_geoms = raw_geom_data_pe
            ),
            geouid = GeoUID,
            time = Date
          )
        )
    }
  ),

  #' ---
  #' DATA PROCESSING ----------------------------
  #' ---
  tar_target(name = on_fort,
    command = {
      tidy_geom(raw_geom_data_on)
    }
  ),
  tar_target(name = mb_fort,
    command = {
      tidy_geom(raw_geom_data_mb)
    }
  ),
  tar_target(name = sk_fort,
    command = {
      tidy_geom(raw_geom_data_sk)
    }
  ),
  tar_target(name = ab_fort,
    command = {
      tidy_geom(raw_geom_data_ab)
    }
  ),
  tar_target(name = qc_fort,
    command = {
      tidy_geom(raw_geom_data_qc)
    }
  ),
  tar_target(name = bc_fort,
    command = {
      tidy_geom(raw_geom_data_bc)
    }
  ),
  tar_target(name = nu_fort,
    command = {
      tidy_geom(raw_geom_data_nu)
    }
  ),
  tar_target(name = nt_fort,
    command = {
      tidy_geom(raw_geom_data_nt)
    }
  ),
  tar_target(name = yt_fort,
    command = {
      tidy_geom(raw_geom_data_yt)
    }
  ),
  tar_target(name = nb_fort,
    command = {
      tidy_geom(raw_geom_data_nb)
    }
  ),
  tar_target(name = nl_fort,
    command = {
      tidy_geom(raw_geom_data_nl)
    }
  ),
  tar_target(name = pe_fort,
    command = {
      tidy_geom(raw_geom_data_pe)
    }
  ),
  tar_target(name = tidy_weather_station_data,
    command = {
      tidy_climate(raw_station_data)
    }
  )
)
