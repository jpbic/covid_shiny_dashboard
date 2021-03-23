# required_packages = c(
#   'dplyr',
#   'countrycode'
# )
# lapply(required_packages, require, character.only=T)
library(dplyr)
library(countrycode)

global_graph_data = read.csv('./data/all_data_with_iso.csv') %>%
  mutate(date = as.Date(date), list_country = countrycode(iso3, 'iso3c', 'country.name.en'))

us_graph_data = read.csv('./data/us_data.csv') %>%
  mutate(date = as.Date(date)) %>%
  mutate(list_country = Province_State)

metric_set_choices = list(
  'Cumulative',
  'Rolling Average Change',
  'Mortality Rate'
)
