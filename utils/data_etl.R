# Data sourced from Kaggle: 
# https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset
required_packages = c(
  'dplyr',
  'tmaptools',
  'countrycode',
  'tidyr',
  'lubridate',
  'usmap',
  'sf'
)
lapply(required_packages, require, character.only=T)

total_global = build_global_frame()
write.csv(total_global, file='./data/all_data_with_iso.csv', row.names=F)

write.csv(build_us_frame(), file='./data/us_data.csv', row.names=F)
  
build_global_frame = function() {
  total = data.frame()
  global_pop = select(filter(world_bank_pop, indicator=='SP.POP.TOTL'), country, `2017`)
  for (t in c('confirmed', 'deaths', 'recovered')) {
    df = read.csv(paste0('./data/time_series_covid_19_', t, '.csv'), stringsAsFactors = F) %>%
      rename_with(~gsub('(\\.)([0-2]{2})([0-2]{2})$', '\\1\\3', .x)) %>%
      filter(
        Lat != 0 | Long != 0, 
        !is.na(Lat) & !is.na(Long), 
        Country.Region != 'Micronesia',
        Province.State != 'French Polynesia'
      ) %>%
      add_row(Country.Region='North Korea', Lat=39.0392, Long=125.7625) %>%
      pivot_df(type=t, Country.Region, Lat, Long)
    
    if (dim(total)[1] == 0) {
      total = df
    } else {
      total = full_join(total, df)
    }
  }
  
  total = total %>%
    add_iso_to_df %>%
    mutate(across(c(confirmed, deaths, recovered), ~replace_na(.x, 0))) %>%
    group_by(iso3, date) %>%
    summarise(across(c(confirmed, deaths, recovered), sum)) %>%
    ungroup() %>%
    inner_join(global_pop, by=c('iso3'='country')) %>%
    filter(!is.na(iso3), !is.na('2017'))
  
  return(total)
}

build_us_frame = function() {
  data(statepop)
  us_pop = select(statepop, full, pop_2015)
  total = data.frame()
  for(t in c('confirmed', 'deaths')) {
    df = read.csv(paste0('./data/time_series_covid_19_', t, '_US.csv'), stringsAsFactors=F) %>%
      pivot_df(type=t, Province_State)
    
    if (dim(total)[1] == 0) {
      total = df
    } else {
      total = full_join(total, df)
    }
  }
  
  total = total %>%
    left_join(us_pop, by=c('Province_State'='full'))
  
  return(total)
}

pivot_df = function(df, type, ...) {
  return(df %>%
    group_by(...) %>%
    summarise(across(starts_with('X'), sum)) %>%
    ungroup() %>%
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
