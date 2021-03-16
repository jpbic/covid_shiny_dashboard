required_packages = c(
  'dplyr',
  'tmaptools',
  'countrycode',
  'sf'
)
lapply(required_packages, require, character.only=T)

confirmed = read.csv('./data/time_series_covid_19_confirmed.csv', stringsAsFactors = T)
confirmed = clean_df(confirmed) %>% add_iso_to_df %>%
  rename_with(~gsub('(\\.)([0-2]{2})([0-2]{2})$', '\\1\\3', .x))
deaths = read.csv('./data/time_series_covid_19_deaths.csv', stringsAsFactors = T)
deaths = clean_df(deaths) %>% add_iso_to_df
recovered = read.csv('./data/time_series_covid_19_recovered.csv', stringsAsFactors = T)
recovered = clean_df(recovered) %>% add_iso_to_df

write.csv(rbind(confirmed, deaths, recovered), file='./data/all_data_with_iso.csv', row.names=F)

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
      select(-Province.State, -Country.Region) %>%
      mutate(type = type)
  )
}

add_iso_to_df = function(df) {
  sf_df = st_as_sf(df, crs='EPSG:4326', coords=c('Long', 'Lat'))
  ccodes = rev_geocode_OSM(sf_df, as.data.frame=T) %>%
    select(iso3 = country_code) %>%
    mutate(iso3 = countrycode(toupper(iso3), 'iso2c', 'iso3c'))
  return(cbind(df, ccodes))
}
