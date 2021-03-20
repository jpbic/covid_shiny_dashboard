# Data sourced from Kaggle: 
# https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset
required_packages = c(
  'dplyr',
  'tmaptools',
  'countrycode',
  'sf'
)
lapply(required_packages, require, character.only=T)

confirmed = read.csv('./data/time_series_covid_19_confirmed.csv', stringsAsFactors = T) %>%
  rename_with(~gsub('(\\.)([0-2]{2})([0-2]{2})$', '\\1\\3', .x))
confirmed = clean_df(confirmed)
deaths = read.csv('./data/time_series_covid_19_deaths.csv', stringsAsFactors = T)
deaths = clean_df(deaths)
recovered = read.csv('./data/time_series_covid_19_recovered.csv', stringsAsFactors = T)
recovered = clean_df(recovered)

total = full_join(confirmed, deaths) %>% full_join(recovered) %>%
  add_iso_to_df %>%
  mutate(across(c(confirmed, deaths, recovered), ~replace_na(.x, 0))) %>%
  group_by(iso3, date) %>%
  summarise(across(c(confirmed, deaths, recovered), sum)) %>%
  ungroup()

write.csv(total, file='./data/all_data_with_iso.csv', row.names=F)

confirmed_us = read.csv('./data/time_series_covid_19_confirmed_US.csv', stringsAsFactors = T) %>%
  select(Province_State, starts_with('X')) %>%
  pivot_longer(starts_with('X'), names_to='date', values_to='confirmed') %>%
  mutate(date = as.Date(gsub('X', '', date), '%m.%d.%y')) %>%
  group_by(Province_State, date) %>%
  summarise(confirmed=sum(confirmed)) %>%
  ungroup()
deaths_us = read.csv('./data/time_series_covid_19_deaths_US.csv', stringsAsFactors = T) %>%
  select(Province_State, starts_with('X')) %>%
  pivot_longer(starts_with('X'), names_to='date', values_to='deaths') %>%
  mutate(date = as.Date(gsub('X', '', date), '%m.%d.%y')) %>%
  group_by(Province_State, date) %>%
  summarise(deaths=sum(deaths)) %>%
  ungroup()

write.csv(full_join(confirmed_us, deaths_us), file='./data/us_data.csv', row.names=F)

clean_df = function(df) {
  type = deparse(substitute(df))
  return(
    df %>%
      filter(
        Lat != 0 | Long != 0, 
        !is.na(Lat) & !is.na(Long), 
        Country.Region != 'Micronesia',
        Province.State != 'French Polynesia'
      ) %>%
      group_by(Country.Region, Lat, Long) %>%
      summarise(across(starts_with('X'), sum)) %>%
      ungroup() %>%
      add_row(Country.Region='North Korea', Lat=39.0392, Long=125.7625) %>%
      mutate(across(starts_with('X'), ~replace_na(.x, 0))) %>%
      pivot_longer(starts_with('X'), names_to='date', values_to=type) %>%
      mutate(date = as.Date(gsub('X', '', date), '%m.%d.%y'))
  )
}

add_iso_to_df = function(df) {
  sf_df = select(df, Long, Lat) %>%
    distinct() %>%
    st_as_sf(crs='EPSG:4326', coords=c('Long', 'Lat'), remove=F)
  ccodes = rev_geocode_OSM(sf_df, as.data.frame=T) %>%
    select(iso3 = country_code) %>%
    mutate(iso3 = countrycode(toupper(iso3), 'iso2c', 'iso3c'))
  sf_df = st_drop_geometry(sf_df) %>%
    cbind(ccodes)
  
  return(left_join(df, sf_df))
}
