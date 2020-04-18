metro_fips <- get_metro_census()
ts_us <- get_metro_ts()
save(metro_fips, file = "./data/metro.RDA")
save(ts_us, file = "./data/ts.RDA") 
