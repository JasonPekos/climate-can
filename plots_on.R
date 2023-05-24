on_sp = split(on_ts_full, factor(on_ts_full$s))
on_t = split(on_ts_full, factor(on_ts_full$t))

nt_on = length(on_t)
nsp_on = length(on_sp)

do.call()

#' vector of season names

seasons = c("winter", "spring", "summer", "fall")


ns = length(unique(pe_ts_full$GeoUID)) #No of regions
nt = length(unique(pe_ts_full$Date)) #No of time points

pe_ts_full <- pe_ts_full %>%
  mutate(t = rep(seq(1:nt), ns)) %>%
  mutate(s = rep(seq(1,ns), each = nt))



#' Plot avg productivity on all data
#' Avg prod over all regions 
Y_all = numeric()
for(i in 1:length(on_sp)){
  Y_all[i] = mean(on_sp[[i]]$tot_prod, na.rm = T)
}

spdat_on = na.omit(Y_all)
