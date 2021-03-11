required_packages = c(
  'dplyr',
  'tmap',
  'tmaptools',
  'countrycode',
  'sf'
)
lapply(required_packages, require, character.only=T)

add_iso_to_csv = function(input_file, output_file) {
  df = read.csv(input_file) %>% st_as_sf(crs='EPSG:4326', coords=c('Long', 'Lat'))
  ccodes = rev_geocode_OSM(df, as.data.frame=T) %>%
    select(iso3 = country_code) %>%
    mutate(iso3 = countrycode(toupper(iso3), 'iso2c', 'iso3c'))
  df = cbind(st_drop_geometry(df), ccodes)
  write.csv(df, file=output_file, row.names=F)
}
