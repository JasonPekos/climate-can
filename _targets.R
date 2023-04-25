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
  #' READ IN THE RAW DATA ----------------------------
  #' ---

  #' CLIMATE
  tar_target(name = raw_climate_data,
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
  tar_target(name = climate_dat,
    command = {
      tidy_climate(raw_climate_data)
    }
  ),

  #' ---
  #' DATA VISUALIZATION ----------------------------
  #' ---
  tar_target(name = plot_geoms,
  command = {
    theme_set("red")
    ggplot() +
      geom_polygon(data = on_fort,
                   aes(x = long, y = lat, group = group),
                   fill = "#69b3a2",
                   color = "white") +
      geom_polygon(data = mb_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = sk_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = ab_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = qc_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = bc_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = nu_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = nt_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = yt_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = nb_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = nl_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      geom_polygon(data = pe_fort,
                    aes(x = long, y = lat, group = group),
                    fill = "#69b3a2",
                    color = "white") +
      coord_map() +
      theme_void()
  }),
  tar_target(name = plot_geoms_multi,
  command = {
    fill = "#B97C7C"
    color = "#7C0000"

    p_bc <- plot_geom(bc_fort, color = color, fill = fill)
    p_on <- plot_geom(on_fort, color = color, fill = fill)
    p_ab <- plot_geom(ab_fort, color = color, fill = fill)
    p_mb <- plot_geom(mb_fort, color = color, fill = fill)
    p_nl <- plot_geom(nl_fort, color = color, fill = fill)
    p_nb <- plot_geom(nb_fort, color = color, fill = fill)
    p_nt <- plot_geom(nt_fort, color = color, fill = fill)
    p_nu <- plot_geom(nu_fort, color = color, fill = fill)
    p_pe <- plot_geom(pe_fort, color = color, fill = fill)
    p_qc <- plot_geom(qc_fort, color = color, fill = fill)
    p_sk <- plot_geom(sk_fort, color = color, fill = fill)
    p_yt <- plot_geom(yt_fort, color = color, fill = fill)

    (p_yt | p_nt | p_nu) / (p_bc | p_ab | p_sk ) / (p_mb | p_on | p_qc ) / ( p_nl | p_nb | p_pe) # nolint
  }
  ),
  tar_target(name = plot_geoms_stations_on,
  command = {
    plot_geom(on_fort) + 
      geom_point(data = climate_dat, aes(x = Longitude..x., y = Latitude..y.))
  }
  ),
  tar_quarto(report, "Scratch.qmd")
)
