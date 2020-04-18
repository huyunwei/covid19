suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(httr)
  library(tidycensus)
  library(lubridate)
  })

get_jhu_covid_usts <- function(jhu_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
                               jhu_confirmed = "time_series_covid19_confirmed_US.csv",
                               jhu_death = "time_series_covid19_deaths_US.csv")

{
  # get JHU covid US time series data
  ts_confirmed_us_url <- paste0(jhu_url, jhu_confirmed)
  ts_confirmed_us <- read_csv(ts_confirmed_us_url, 
                              col_types = cols(.default = "c"))
  key_cols <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_")
  ts_us <- ts_confirmed_us %>% 
    select(-one_of(key_cols)) %>% 
    pivot_longer(-Combined_Key, names_to = "date", 
                 values_to = "confirmed") %>%
    mutate(date = mdy(date),
           confirmed = as.integer(confirmed)) %>% 
    janitor::clean_names() 
  
  ts_death_us_url <- paste0(jhu_url, jhu_death)
  ts_death_us <- read_csv(ts_death_us_url, 
                          col_types = cols(.default = "c"))
  key_cols <- c(key_cols, "Population")
  us_keys <- ts_death_us %>% 
    select(c(one_of(key_cols), "Combined_Key")) %>% 
    distinct() %>% 
    janitor::clean_names() %>% 
    mutate(fips = as.integer(fips))
  
  ts_us <- ts_death_us %>% 
    select(-one_of(key_cols)) %>% 
    pivot_longer(-Combined_Key, names_to = "date", 
                 values_to = "deaths") %>%
    mutate(date = mdy(date),
           deaths = as.integer(deaths)) %>% 
    janitor::clean_names() %>% 
    left_join(ts_us, by = c("combined_key", "date")) %>% 
    left_join(select(us_keys, combined_key, fips, population, lat, long),
              by = "combined_key")
}



 

get_metro_census <- function(census_year = 2018,
                             metro_url = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls",
                             geo = "county") {
  # function to create census data based on metro areas
  # The OMB Metropolititan delineation can be downloaded from [Census](https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html).
  # return a tibble with metro, fips, population. 
  # TODO expand to get more census data.

  census_api_key("d26bdb323e1a7649867b33225248dc93a8d3fb51")
  
  us_pop <- get_acs(geography = geo, 
                    variables = c(population = "B01001_001E"), 
                    # state = "TX",
                    geometry = TRUE,
                    year = census_year)
  
  
  GET(metro_url, write_disk(tf <- tempfile(fileext = ".xls")))
  metro <- read_excel(tf,
                      col_types = "text", 
                      skip = 2) %>% 
    select(-`Metropolitan/Micropolitan Statistical Area`) %>% 
    janitor::clean_names() %>% 
    mutate(fips = str_c(`fips_state_code`, `fips_county_code`))
  
  metro_fips <- metro %>% 
    filter(!is.na(csa_title)) %>%
    select(metro = csa_title, fips) 
  
  metro_pop <- us_pop %>% 
    as_tibble() %>% 
    select(fips = GEOID, population = estimate) %>% 
    right_join(metro_fips, by = "fips") %>% 
    mutate(fips = as.numeric(fips))
}



get_metro_ts <- function(metro_fips){
  ts_us <- get_jhu_covid_usts()
  
  metro_pop_jhu <- ts_us %>% 
    select(fips, population, lat, long) %>%
    distinct() %>% 
    left_join(select(metro_fips, metro, fips),by = "fips") %>% 
    group_by(metro) %>% 
    summarise(population = sum(as.numeric(population)),
              lat = mean(as.numeric(lat)),
              long =mean(as.numeric(long))) %>% 
    ungroup() %>% 
    arrange(desc(population))
  
  metro_ts <- metro_fips %>% 
    mutate(fips = as.numeric(fips)) %>% 
    select(metro, fips) %>% 
    right_join(ts_us, by = "fips") %>% 
    group_by(metro, date) %>%
    arrange(desc(date)) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% 
    filter(!metro == "NA") %>% 
    filter(confirmed > 10) %>% 
    mutate(days_from_nconfirmed = row_number(),
           days_from_ndeath = cumsum(deaths > 2),
           daily_new_case = confirmed - lag(confirmed),
           daily_new_case_3 = (confirmed - lag(confirmed,3))/3,
           daily_new_death = deaths - lag(deaths),
           daily_new_death_3 = (deaths - lag(deaths,3))/3) %>% 
    ungroup() %>% 
    left_join(metro_pop_jhu, by = "metro")
}