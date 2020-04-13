# function to create census data based on metro areas
# The OMB Metropolititan delineation can be downloaded from [Census](https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html).
# return a tibble with metro, fips, population. 
#TODO expand to get more census data. 

get_metro_census <- function(census_year = 2018,
                             metro_url = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls",
                             geo = "county") {
library(tidyverse)
library(readxl)
library(httr)

library(tidycensus)
census_api_key("d26bdb323e1a7649867b33225248dc93a8d3fb51")

us_pop <- get_acs(geography = geo, 
                  variables = c(population = "B01001_001E"), 
                  # state = "TX",
                  geometry = TRUE,
                  year = census_year)


GET(metro_url, write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf, 2L)


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
  right_join(metro_fips, by = "fips")
}