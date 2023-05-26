library(targets)
library(profvis)
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(sf)
library(imputeTS)
library(spdep)
library(lubridate)
library(ggpubr)
library(animation)
##########
## MONTHLY
##########
## PEI diff plot
## Yearly
prod <- tar_read(preds_pe) %>%
  mutate(Year = year(Date)) %>%
  select(GeoUID, s, tot_prod, pred_h, pred_l, Year)

census <- tar_read(raw_geom_data_pe)
year_of_interest <- 2008  # replace with your desired date
rastc <- unwrap(tar_read(cmip5_high_temp))

prod_sub <- prod %>%
  filter(Year == year_of_interest) %>%
  mutate(GeoUID = as.character(GeoUID)) %>%
  select(GeoUID, s, tot_prod, pred_h, pred_l, Year)

avg_prod <- prod_sub %>%
  group_by(GeoUID) %>%
  summarise(true_avg_sp_prod = mean(tot_prod, na.rm = TRUE),
            pred_h_avg_sp_prod = mean(pred_h, na.rm = TRUE),
            pred_l_avg_sp_prod = mean(pred_l, na.rm = TRUE))


# Join the spatial data with the temperature data
pe_sf <- left_join(census, avg_prod, by = "GeoUID")

#limits of PEI
lim_pe_true = range(pe_sf$true_avg_sp_prod, na.rm = T)
lim_pe_pred = range(pe_sf$pred_h_avg_sp_prod, na.rm = T)

lim = c(0, max(lim_pe_true[2], lim_pe_pred[2]))
  
##PE TRUE
plot_true_prod_pe <- ggplot() +
  geom_sf(data = pe_sf, aes(fill = true_avg_sp_prod), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Tot Prod", title = paste("True Overall Productivity at", year_of_interest)) +
  theme_void()

##PE PRED
plot_pred_prod_pe <- ggplot() +
  geom_sf(data = pe_sf, aes(fill = pred_h_avg_sp_prod), color ="black") +
  scale_fill_gradient(low = "blue", high = "red", limits = lim) +
  theme_minimal() +
  labs(fill = "Tot Prod", title = paste("Predicted Overall Productivity at", year_of_interest)) +
  theme_void()


ggarrange(plot_true_prod_pe, plot_pred_prod_pe,  common.legend = TRUE)


## ANIMATION with Year ##
prod <- tar_read(preds_pe) %>%
  mutate(Year = year(Date)) %>%
  select(GeoUID, Date, s, t, tot_prod, pred_h, pred_l, Year)

census <- tar_read(raw_geom_data_pe)

#Set limits
lim = c(min(prod$tot_prod, na.rm = TRUE), max(prod$tot_prod, na.rm = TRUE))

Preds_year <- function(time) {
  
    prod_sub <- prod %>%
    filter(t == time) %>%
    mutate(GeoUID = as.character(GeoUID)) %>%
    select(Date, GeoUID, t, s, tot_prod, pred_h, pred_l, Year)
  
    yr = unique(prod_sub$Date)
      
    avg_prod <- prod_sub %>%
    group_by(GeoUID) %>%
    summarise(true_avg_sp_prod = mean(tot_prod, na.rm = TRUE),
              pred_h_avg_sp_prod = mean(pred_h, na.rm = TRUE),
              pred_l_avg_sp_prod = mean(pred_l, na.rm = TRUE))
  
  
  # Join the spatial data with the temperature data
  pe_sf <- left_join(census, avg_prod, by = "GeoUID")
  
  ##PE TRUE
  plot_true_prod_pe <- ggplot() +
    geom_sf(data = pe_sf, aes(fill = true_avg_sp_prod), color ="black") +
    scale_fill_gradient(low = "blue", high = "red", limits = lim) +
    theme_minimal() +
    labs(fill = "Tot Prod", title = paste("True Overall Productivity at", yr)) +
    theme_void()
  
  ##PE PRED
  plot_pred_prod_pe <- ggplot() +
    geom_sf(data = pe_sf, aes(fill = pred_h_avg_sp_prod), color ="black") +
    scale_fill_gradient(low = "blue", high = "red", limits = lim) +
    theme_minimal() +
    labs(fill = "Tot Prod", title = paste("Predicted Overall Productivity at", yr)) +
    theme_void()
  
  ggarrange(plot_true_prod_pe, plot_pred_prod_pe, common.legend = TRUE)
  
}

## -----------------------------------------------------------

gen_anim <- function() {
  for(t in 109:157){  # for each year
    plot(Preds_year(t))           # plot region at this year
  }
}

ani.options(interval = 0.5)     # 0.2s interval between frames
saveHTML(gen_anim(),            # run the main function
         autoplay = FALSE,      # do not play on load
         loop = FALSE,          # do not loop
         verbose = FALSE,       # no verbose
         outdir = ".",          # save to current dir
         single.opts = "'controls': ['first', 'previous',
                                    'play', 'next', 'last',
                                     'loop', 'speed'],
                                     'delayMin': 0",
         htmlfile = "PEI_anim.html")  # save filename



avg_prod <- pred %>%
  group_by(t) %>%
  summarise(true_avg_sp_prod = mean(tot_prod, na.rm = TRUE),
            pred_h_avg_sp_prod = mean(pred_h_med, na.rm = TRUE),
            pred_l_avg_sp_prod = mean(pred_l_med, na.rm = TRUE))

ggplot(avg_prod) +
  geom_point(aes(x = 1:nrow(avg_prod), y = true_avg_sp_prod)) +
  geom_line(aes(x = 1:nrow(avg_prod), y = pred_l_avg_sp_prod), col = "blue") +
  geom_line(aes(x = 1:nrow(avg_prod), y = pred_h_avg_sp_prod), col = "red") +
  scale_x_continuous(breaks = seq(1, nrow(avg_prod), by = 12))

