suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  library(readxl)
  library(httr)
  library(tidycensus)
  library(lubridate)
  library(conflicted)
  library(chromote)
  })
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

get_appleMobility_chrom <- function(){
  b <- ChromoteSession$new()
  b$Page$navigate("https://www.apple.com/covid19/mobility")
  Sys.sleep(5)
  x <- b$DOM$getDocument()
  x <- b$DOM$querySelector(x$root$nodeId, ".download-button-container")
  x <- b$DOM$getOuterHTML(x$nodeId)$outerHTML
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  url <- str_extract(string = x, pattern = url_pattern)
  apple_mobility <- read_csv(file = url, col_types = cols(.default = "c")) %>% 
    pivot_longer(cols = starts_with("2020-"), 
                 names_to = "date", 
                 values_to = "index") %>% 
    mutate(date = lubridate::ymd(date),
           index = as.numeric(index))
}

get_appleMobility <- function(){
  getcmd = 'x=$(curl -s https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json); echo https://covid19-static.cdn-apple.com`jq -n "$x" |jq -r \'.basePath\'``jq -n "$x"|jq -r \'.regions."en-us".csvPath\'`'
  url = system(getcmd, intern = T)
  apple_mobility <- read_csv(file = url, col_types = cols(.default = "c")) %>% 
    pivot_longer(cols = starts_with("2020-"), 
                 names_to = "date", 
                 values_to = "index") %>% 
    mutate(date = lubridate::ymd(date),
           index = as.numeric(index))
}
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
                             metro_url = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls",
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



get_metro_ts <- function(metro_fips, per_cap = FALSE, ma_len = 7){
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
           daily_new_case_ma = (confirmed - lag(confirmed,ma_len))/ma_len,
           daily_new_death = deaths - lag(deaths),
           daily_new_death_ma = (deaths - lag(deaths,ma_len))/ma_len) %>% 
    ungroup() %>% 
    left_join(metro_pop_jhu, by = "metro") %>% 
    mutate(confirmed_per1000 = confirmed / population * 1000,
           deaths_per1000 = deaths / population * 1000,
           daily_new_case_per1000 = daily_new_case / population * 1000,
           daily_new_case_ma_per1000 = daily_new_case_ma / population * 1000,
           daily_new_death_per1000 = daily_new_death / population * 1000,
           daily_new_death_ma_per1000 = daily_new_death_ma / population * 1000)
  
  if(per_cap){
    metro_ts <- metro_ts %>% 
      mutate(confirmed = confirmed_per1000,
           deaths = deaths_per1000,
           daily_new_case = daily_new_case_per1000,
           daily_new_case_ma = daily_new_case_ma_per1000,
           daily_new_death = daily_new_death_per1000,
           daily_new_death_ma = daily_new_death_ma_per1000)
  }
  metro_ts
}




#' RKI R_e(t) estimator with generation time GT (default GT=4)
#' @param ts - Vector of integer values containing the time series of incident cases
#' @param GT - PMF of the generation time is a fixed point mass at the value GT.
est_rt_rki <- function(ts, GT=4L) {
  # Sanity check
  if (!is.integer(GT) | !(GT>0)) stop("GT has to be postive integer.")
  # Estimate
  sapply( (1+GT):(length(ts)-GT), function(t) {
    sum(ts[t+(0:(GT-1))]) / sum(ts[t-(1:GT)]) 
  })
}

get_metro_rt_rki <- function(ts, show_metro){
  last_report <- ts %>% 
    group_by(metro) %>% 
    summarise(last_updated = max(date)) %>% 
    ungroup()
  
  metro_rt_rki <- ts %>% 
    filter(metro %in% show_metro) %>% 
    filter(confirmed > 0) %>% 
    # group_by(metro) %>% 
    split(.$metro) %>% 
    map(~ est_rt_rki(.x$daily_new_case_ma, GT = 4L)) %>% 
    enframe() %>% 
    unnest() %>% 
    drop_na() %>% 
    rename(metro = name, rt = value ) %>% 
    left_join(last_report, by = "metro") %>% 
    group_by(metro) %>% 
    mutate(days = last_updated - days(n() - row_number())) %>% 
    ungroup()
}

get_covidtracking_states <- function(states_url = "https://covidtracking.com/api/v1/states/daily.json"
) {
  fromJSON(states_url) %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    mutate(pos_perc = positiveIncrease / totalTestResultsIncrease)
}