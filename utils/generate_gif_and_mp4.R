required_packages = c(
  'dplyr',
  'tmap',
  'lubridate',
  'gifski',
  'tidyr',
  'slider',
  'sf'
)
lapply(required_packages, require, character.only = T)

data(World)
pal = c('#D9EF8B', '#FEE08B', '#FDAE61', '#F46D43', '#D73027')

generate_gif = function(data_col_str, input_file, output_file, legend_title, gif_delay,
                        facet_along, polygon_col) {
  sf_df = create_map_data(input_file, data_col_str)
  dims = get_dims(sf_df, facet_along, polygon_col)
  save_gif(get_maps(sf_df, legend_title), height=dims[1], width=dims[2], delay=gif_delay,
           gif_file=output_file, loop=T)
}

generate_mp4 = function(data_col_str, input_file, output_file, legend_title, fps) {
  sf_df = create_map_data(input_file, data_col_str)
  tmap_animation(get_maps_mp4(sf_df, legend_title), filename=output_file, fps=fps, 
                 width=1044, height=470)
}

create_map_data = function(input_file, data_col_str) {
  message('Creating SF Object')
  
  sf_df = read.csv(input_file, stringsAsFactors = T) %>%
    select(-Province.State, -Country.Region) %>%
    pivot_longer(!iso3, names_to = 'date', values_to = 'confirmed') %>%
    mutate(date = as.Date(gsub('X', '', date), '%m.%d.%Y')) %>%
    group_by(iso3, date) %>%
    summarise(data_col = sum(!!as.symbol(data_col_str))) %>%
    mutate(change = data_col - lag(data_col, n = 1, default=0, order_by=iso3, date)) %>%
    mutate(rolling_ave_change = slide_dbl(change, mean, .before=7, .after=7)) %>%
    mutate(change_delta = change - lag(change, n = 1, default=0, order_by=iso3, date)) %>%
    mutate(rolling_ave_change_delta = slide_dbl(change_delta, mean, .before=7, .after=7)) %>%
    mutate(perc_change = ifelse(
      rolling_ave_change == 0 | data_col < 1000,
      0,
      100 * rolling_ave_change_delta / abs(rolling_ave_change)
    )) %>%
    inner_join(select(World, iso_a3, geometry), by=c('iso3' = 'iso_a3')) %>%
    st_sf()
  message('SF Object Created')
  return(sf_df)
}

get_dims = function(df, map_slice_col, polygon_col) {
  slice = st_drop_geometry(select(df, !!as.symbol(map_slice_col)))[[1]][1]
  t = tm_shape(filter(df, !!as.symbol(map_slice_col) == slice)) +
    tm_polygons(polygon_col)
  filename = tempfile(fileext='.png')
  tmap_save(t, filename=filename)
  dims = dim(png::readPNG(filename))
  file.remove(filename)
  
  return(dims)
}

get_maps = function(sf_df, legend_title) {
  message('Building Maps')
  
  uniq_dates = sort(unique(sf_df$date))
  for (i in 1:length(uniq_dates)) {
    t = tm_shape(filter(sf_df, date==uniq_dates[i])) +
      tm_polygons('perc_change', colorNA = NULL, palette=pal, 
                  title=legend_title, style='fixed',
                  breaks = c(-Inf, -3, 3, 10, 25, Inf)
      ) +
      tm_layout(
        main.title = as.character(uniq_dates[i]), 
        main.title.position = 'center',
        bg.color = '#b0f7ff', 
        main.title.size = 5,
        legend.position = c('left', 'bottom'),
        legend.text.size = 3,
        legend.title.size = 4,
        legend.bg.color = '#FFFFFF',
        legend.frame = '#000000'
      )
    print(t)
  }
  
  message('Generating GIF')
}

get_maps_mp4 = function(sf_df, legend_title) {
  message('Building Maps')
  
  t = tm_shape(sf_df) +
    tm_polygons('perc_change', colorNA = NULL, palette=pal, 
                title=legend_title, style='fixed',
                breaks = c(-Inf, -3, 3, 10, 25, Inf)
    ) +
    tm_layout(
      main.title.position = 'center',
      bg.color = '#b0f7ff', 
      main.title.size = 1,
      legend.position = c('left', 'bottom'),
      legend.text.size = 1,
      legend.title.size = 0.75,
      legend.bg.color = '#FFFFFF',
      legend.frame = '#000000',
      frame = F
    ) +
    tm_facets(along='date', free.coords = F, nrow = 1, ncol = 1)
  
  message('Generating MP4')
  return(t)
}

# generate_gif('confirmed', './data/time_series_covid_19_confirmed_w_iso.csv', 
#              './www/conf_anim2.gif', '% Change in New Confirmed Cases', 0.125,
#              'date', 'perc_change')
# 
# generate_mp4('confirmed', './data/time_series_covid_19_confirmed_w_iso.csv', 
#              './www/conf_anim.mp4', '% Change in New Confirmed Cases', 8)
