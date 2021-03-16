required_packages = c(
  'dplyr',
  'tmap',
  'lubridate',
  'gifski',
  'tidyr',
  'slider',
  'sf',
  'RColorBrewer'
)
lapply(required_packages, require, character.only = T)

data(World)
conf_pal = c('#dbe4bc', '#f4f2f0', '#f4dac4', '#f4c7a6', '#e08b60')
pop_pal = c("#f4f2f0", "#f4dac4", "#f4c7a6", "#e08b60","#D1A490")
conf_breaks = c(-Inf, -3, 3, 10, 25, Inf)
pop_breaks = c(0, 0.5, 2, 5, 10, Inf)

generate_gif = function(input_file, output_file, legend_title, gif_delay,
                        facet_along, polygon_col, breaks, pal, type) {
  sf_df = create_map_data(input_file, type)
  dims = get_dims(sf_df, facet_along, polygon_col)
  save_gif(get_maps(sf_df, legend_title, polygon_col, breaks, pal, basemap), height=dims[1], 
           width=dims[2], delay=gif_delay, gif_file=output_file, loop=T)
}

generate_mp4 = function(input_file, output_file, legend_title, fps,
                        polygon_col, breaks, pal, type) {
  sf_df = create_map_data(input_file, type)
  tmap_animation(get_maps_mp4(sf_df, legend_title, polygon_col, breaks, pal), 
                 filename=output_file, fps=fps, width=1044, height=470)
}

create_map_data = function(input_file, type) {
  message('Creating SF Object')
  
  pop = filter(world_bank_pop, indicator == 'SP.POP.TOTL') %>%
    select(country, population = `2017`)
  
  sf_df = read.csv(input_file, stringsAsFactors = T) %>%
    filter(type == type) %>%
    pivot_longer(!type:iso3, names_to = 'date', values_to = 'data_col') %>%
    mutate(date = as.Date(gsub('X', '', date), '%m.%d.%y')) %>%
    group_by(iso3, date) %>%
    summarise(data_col = sum(data_col)) %>%
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
    left_join(pop, by=c('iso3' = 'country')) %>%
    mutate(perc_pop = 100 * data_col / population) %>%
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

get_maps = function(sf_df, legend_title, polygon_col, breaks, pal) {
  message('Building Maps')
  
  uniq_dates = sort(unique(sf_df$date))
  for (i in 1:length(uniq_dates)) {
    t = tm_shape(filter(sf_df, date==uniq_dates[i])) +
      tm_polygons(polygon_col, colorNA = NULL, palette=pal, 
                  title=legend_title, style='fixed',
                  breaks = breaks, 
                  alpha=1,
                  border.col='#642102',
                  border.alpha = 0.05
      ) +
      tm_layout(
        main.title = as.character(uniq_dates[i]), 
        main.title.position = 'center',
        bg.color = '#dddddd', 
        main.title.size = 5,
        space.color = 'transparent',
        legend.position = c('left', 'bottom'),
        legend.text.size = 2,
        legend.title.size = 4,
        legend.bg.color = '#FFFFFF',
        legend.bg.alpha = 0.7,
        saturation = 1.1,
        frame = F
      )
    print(t)
  }
  
  message('Generating GIF')
}

get_maps_mp4 = function(sf_df, legend_title, polygon_col, breaks, pal, basemap) {
  message('Building Maps')
  
  t = tm_shape(sf_df) +
    tm_polygons(polygon_col, 
                colorNA = NULL, 
                palette=pal, 
                title=legend_title, 
                style='fixed',
                breaks = breaks, 
                alpha=1,
                border.col='#642102',
                border.alpha = 0.05
    ) +
    tm_layout(
      main.title.position = 'center',
      bg.color = '#dddddd', 
      main.title.size = 1,
      legend.position = c('left', 'bottom'),
      legend.text.size = 1,
      legend.title.size = 0.75,
      legend.bg.color = '#FFFFFF',
      legend.bg.alpha = 0.7,
      frame = F
    ) +
    tm_facets(along='date', free.coords = F, nrow = 1, ncol = 1)
  
  message('Generating MP4')
  return(t)
}

# generate_mp4('./data/all_data_with_iso.csv',
#              './www/conf_anim.mp4', '% Change in New Confirmed Cases', 8,
#              'perc_change', conf_breaks, conf_pal, 'confirmed')
