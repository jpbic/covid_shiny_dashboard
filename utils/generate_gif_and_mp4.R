required_packages = c(
  'dplyr',
  'tmap',
  'tmaptools',
  'lubridate',
  'gifski',
  'slider',
  'sf',
  'RColorBrewer',
  'tidyr'
)
lapply(required_packages, require, character.only = T)

source('./utils/config/mp4_config.R')

generate_mp4 = function(
  input_file, 
  output_file, 
  type, 
  group_by_cols,
  geom_join_df,
  geom_left_join_by,
  geom_right_join_by,
  legend_title=NA, 
  breaks=NULL, 
  pal=NULL, 
  bbox=NULL,
  width=1044,
  height=470,
  fps=8
) {
  sf_df = create_map_data(input_file, geom_join_df, geom_left_join_by, geom_right_join_by)
  
  tmap_animation(get_maps_mp4(sf_df, type, legend_title, breaks, pal, bbox), 
                 filename=output_file, fps=fps, width=width, height=height)
}

create_map_data = function(
  input_file, 
  join_df,
  left_join_by,
  right_join_by
) {
  message('Creating SF Object')
  
  join_vector = setNames(right_join_by, left_join_by)
  
  sf_df = read.csv(input_file, stringsAsFactors = F) %>%
    mutate(date=as.Date(date)) %>%
    left_join(join_df, by=join_vector)

  message('SF Object Created')
  return(st_sf(sf_df))
}

get_maps_mp4 = function(
  sf_df, 
  type,
  legend_title, 
  breaks=NULL, 
  pal=NULL, 
  bbox=NULL
) {
  message('Building Maps')
  
  t = tm_shape(sf_df, bbox=bbox) +
    tm_polygons('pop_perc', 
                colorNA = NULL, 
                palette=pal, 
                title=legend_title, 
                style='fixed',
                breaks = breaks, 
                alpha=0.5,
                border.col='#642102',
                border.alpha = 0.05
    ) +
    tm_facets(along='date', free.coords = F, nrow = 1, ncol = 1) +
    tm_shape(sf_df, bbox=bbox) +
    tm_symbols(size = 0.25, col="status_bin", shape=21, breaks=c(-1, 0, 1), 
               palette=c('#91cf60', '#d92f29'), border.col='white', title.col='Status',
               labels=c('Stable', 'Outbreak')) +
    tm_facets(along='date', free.coords = F, nrow = 1, ncol = 1) +
    tm_layout(
      outer.margins = 0,
      main.title.position = 'center',
      bg.color = '#dddddd', 
      main.title.size = 1.5,
      legend.width=0.3,
      legend.height=0.5,
      legend.title.size=1.25,
      legend.text.size=1,
      legend.position = c('left', 'bottom'),
      legend.bg.color = '#FFFFFF',
      legend.bg.alpha = 0.7,
      frame = F
    )
  
  message('Generating MP4')
  return(t)
}

do.call(generate_mp4, us_config)
