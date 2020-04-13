
function <- get_jhu_covid_usts(jhu_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
                               jhu_comfirmed = "time_series_covid19_confirmed_US.csv",
                               juh_death = "time_series_covid19_deaths_US.csv"))

{
  # get JHU covid US time series data
  ts_confirmed_us_url <- paste0(jhu_url, jhu_confirmed)
  ts_confirmed_us <- read_csv(ts_confirmed_us_url, 
                              col_types = cols(.default = "c"))
  us_keys <- ts_confirmed_us %>% 
    select(UID, iso2, iso3, code3, FIPS, Admin2, Province_State,
           Country_Region, Lat, Long_, Combined_Key) %>% 
    distinct() %>% 
    janitor::clean_names() %>% 
    mutate(fips = as.integer(fips))
  key_cols <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_")
  ts_us <- ts_confirmed_us %>% 
    select(-one_of(key_cols)) %>% 
    pivot_longer(-Combined_Key, names_to = "date", 
                 values_to = "confirmed") %>%
    mutate(date = mdy(date),
           confirmed = as.integer(confirmed)) %>% 
    janitor::clean_names() 
  
  ts_death_us_url <- paste0(jhu_url, jhu_death)
  ts_death_us <- read_csv(ts_confirmed_us_url, 
                          col_types = cols(.default = "c"))
  ts_us <- ts_death_us %>% 
    select(-one_of(key_cols)) %>% 
    pivot_longer(-Combined_Key, names_to = "date", 
                 values_to = "deaths") %>%
    mutate(date = mdy(date),
           deaths = as.integer(deaths)) %>% 
    janitor::clean_names() %>% 
    left_join(ts_us, by = c("combined_key", "date"))
}