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

global_critical_value = 2.25
us_critical_value = 9.25

total_global = build_global_frame()
write.csv(total_global, file='./data/all_data_with_iso.csv', row.names=F)

write.csv(build_us_frame(), file='./data/us_data.csv', row.names=F)
  
build_global_frame = function() {
  total = data.frame()
  global_pop = select(filter(world_bank_pop, indicator=='SP.POP.TOTL'), country, population=`2017`)
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
    group_by(Country.Region, iso3, date) %>%
    summarise(across(c(confirmed, deaths, recovered), sum)) %>%
    ungroup() %>%
    inner_join(global_pop, by=c('iso3'='country')) %>%
    filter(!is.na(iso3), !is.na(population)) %>%
    group_by(Country.Region, iso3) %>%
    mutate(
      change_confirmed = confirmed - lag(confirmed, n = 1, default=0, order_by=date),
      change_deaths = deaths - lag(deaths, n=1, default=0, order_by=date),
      change_recovered = recovered - lag(recovered, n=1, default=0, order_by=date)
    ) %>%
    mutate(
      rolling_ave_confirmed = slide_dbl(change_confirmed, mean, .before=7, .after=0),
      rolling_ave_deaths = slide_dbl(change_deaths, mean, .before=7, .after=0),
      rolling_ave_recovered = slide_dbl(change_recovered, mean, .before=7, .after=0)
    ) %>%
    mutate(
      perc_change = 100 *
             slide_dbl(rolling_ave_confirmed - lag(rolling_ave_confirmed, n=1, default=0, order_by=date), sum, .before=3, .after=0) /
             (population * .0005)
    ) %>%
    mutate(pop_perc = 100 * rolling_ave_confirmed / (population * .002)) %>%
    mutate(status = ifelse(
      pmin(pop_perc, perc_change) >= global_critical_value &
        pmin(lag(pop_perc, n=1, default=0, order_by=date),
             lag(perc_change, n=1, default=0, order_by=date)) < global_critical_value,
      'outbreak',
      ifelse(
        pmax(pop_perc, perc_change) < global_critical_value &
          pmax(lag(perc_change, n=1, default=0, order_by=date),
               lag(pop_perc, n=1, default=0, order_by=date)) >= global_critical_value,
        'stable',
        NA
      )
    )) %>%
    mutate(mortality_rate = slide_dbl(deaths, mean, .before=7, .after=0) / slide_dbl(confirmed, mean, .before=7, .after=0)) %>%
    mutate(mortality_rate = replace_na(mortality_rate, 0)) %>%
    ungroup()
  
  total$status[1] = 'stable'
  for (i in 2:nrow(total)) {
    if (total$iso3[i] != total$iso3[i-1]) {
      total$status[i] = 'stable'
    }
    else if (is.na(total$status[i])) {
      total$status[i] = total$status[i-1]
    }
  }
  total = mutate(total, status_bin = ifelse(status=='outbreak', 1, -1)) %>%
    mutate(status = factor(status))
  
  total = mutate(total, across(c(rolling_ave_confirmed, rolling_ave_deaths, 
                                 rolling_ave_recovered, pop_perc, perc_change), 
                               ~replace_na(.x, 0)))
  
  return(total)
}

build_us_frame = function() {
  data(statepop)
  us_pop = select(statepop, full, population=pop_2015)
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
    left_join(us_pop, by=c('Province_State'='full')) %>%
    group_by(Province_State) %>%
    mutate(
      change_confirmed = confirmed - lag(confirmed, n = 1, default=0, order_by=date),
      change_deaths = deaths - lag(deaths, n=1, default=0, order_by=date)
    ) %>%
    mutate(
      rolling_ave_confirmed = slide_dbl(change_confirmed, mean, .before=7, .after=0),
      rolling_ave_deaths = slide_dbl(change_deaths, mean, .before=7, .after=0)
    ) %>%
    mutate(
      perc_change = 100 *
        slide_dbl(rolling_ave_confirmed - lag(rolling_ave_confirmed, n=1, default=0, order_by=date), sum, .before=3, .after=0) /
        (population * .0005)
    ) %>%
    mutate(pop_perc = 100 * rolling_ave_confirmed / (population * .002)) %>%
    mutate(status = ifelse(
      pmin(pop_perc, perc_change) >= us_critical_value &
        pmin(lag(pop_perc, n=1, default=0, order_by=date),
             lag(perc_change, n=1, default=0, order_by=date)) < us_critical_value,
      'outbreak',
      ifelse(
        pmax(pop_perc, perc_change) < us_critical_value &
          pmax(lag(perc_change, n=1, default=0, order_by=date),
               lag(pop_perc, n=1, default=0, order_by=date)) >= us_critical_value,
        'stable',
        NA
      )
    )) %>%
    mutate(mortality_rate = slide_dbl(deaths, mean, .before=7, .after=0) / slide_dbl(confirmed, mean, .before=7, .after=0)) %>%
    mutate(mortality_rate = replace_na(mortality_rate, 0)) %>%
    ungroup()
  
  total$status[1] = 'stable'
  for (i in 2:nrow(total)) {
    if (total$Province_State[i] != total$Province_State[i-1]) {
      total$status[i] = 'stable'
    }
    else if (is.na(total$status[i])) {
      total$status[i] = total$status[i-1]
    }
  }
  total = mutate(total, status_bin = ifelse(status=='outbreak', 1, -1)) %>%
    mutate(status = factor(status))
  
  total = mutate(total, across(c(rolling_ave_confirmed, rolling_ave_deaths, 
                                 pop_perc, perc_change), 
                               ~replace_na(.x, 0)))
  
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
