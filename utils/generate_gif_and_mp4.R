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
  pop_col,
  critical_value,
  legend_title=NA, 
  polygon_col, 
  breaks=NULL, 
  pal=NULL, 
  bbox=NULL,
  width=1044,
  height=470,
  fps=8
) {
  sf_df = create_map_data(input_file, type, group_by_cols, geom_join_df, 
                          geom_left_join_by, geom_right_join_by, pop_col, critical_value)
  
  tmap_animation(get_maps_mp4(sf_df, legend_title, polygon_col, breaks, pal, bbox), 
                 filename=output_file, fps=fps, width=width, height=height)
}

create_map_data = function(
  input_file, 
  type,
  group_by_cols,
  join_df,
  left_join_by,
  right_join_by,
  pop_col,
  critical_value
) {
  message('Creating SF Object')
  
  join_vector = setNames(right_join_by, left_join_by)
  
  sf_df = read.csv(input_file, stringsAsFactors = F) %>%
    mutate(date=as.Date(date)) %>%
    select(!!!syms(group_by_cols), !!!syms(left_join_by), !!as.symbol(type), !!as.symbol(pop_col)) %>%
    rename(data_col = !!as.symbol(type)) %>%
    mutate(change = data_col - lag(data_col, n = 1, default=0, order_by=!!!syms(group_by_cols))) %>%
    mutate(rolling_ave_change = slide_dbl(change, mean, .before=7, .after=0)) %>%
    mutate(change_delta = change - lag(change, n=1, default=0, order_by=!!!syms(group_by_cols))) %>%
    mutate(rolling_ave_change_delta = slide_dbl(change_delta, mean, .before=7, .after=0)) %>%
    left_join(join_df, by=join_vector) %>%
    ungroup() %>%
    mutate(perc_change = 100 *
             slide_dbl(rolling_ave_change - lag(rolling_ave_change, n=1, default=0, order_by=!!!syms(group_by_cols)), sum, .before=3, .after=0) /
             (!!as.symbol(pop_col) * .0005)
    ) %>%
    mutate(pop_perc = 100 * rolling_ave_change / (!!as.symbol(pop_col) * .002)) %>%
    mutate(perc_change_growth = 100 *
             slide_dbl(rolling_ave_change_delta - lag(rolling_ave_change_delta, n=1, default=0, order_by=!!!syms(group_by_cols)), sum, .before=3, .after=0) /
             (!!as.symbol(pop_col) * .0005)
    ) %>%
    mutate(status = ifelse(
      pmin(pop_perc, perc_change) >= critical_value &
        pmin(lag(pop_perc, n=1, default=0, order_by=!!!syms(group_by_cols)),
            lag(perc_change, n=1, default=0, order_by=!!!syms(group_by_cols))) < critical_value,
      'outbreak',
      ifelse(
        pmax(pop_perc, perc_change) < critical_value &
          pmax(lag(perc_change, n=1, default=0, order_by=!!!syms(group_by_cols)),
              lag(pop_perc, n=1, default=0, order_by=!!!syms(group_by_cols))) >= critical_value,
        'stable',
        NA
      )
    ))

  sf_df$status[1] = 'stable'
  for (i in 2:nrow(sf_df)) {
    if (sf_df[i, left_join_by] != sf_df[i-1, left_join_by]) {
      sf_df$status[i] = 'stable'
    }
    else if (is.na(sf_df$status[i])) {
      sf_df$status[i] = sf_df$status[i-1]
    }
  }
  sf_df = mutate(sf_df, status_bin = ifelse(status=='outbreak', 1, -1)) %>%
    mutate(status = factor(status))

  sf_df = mutate(sf_df, across(c(rolling_ave_change, pop_perc, perc_change), ~replace_na(.x, 0)))

  message('SF Object Created')
  return(st_sf(sf_df))
}

get_maps_mp4 = function(
  sf_df, 
  legend_title, 
  polygon_col, 
  breaks=NULL, 
  pal=NULL, 
  bbox=NULL
) {
  message('Building Maps')
  
  t = tm_shape(sf_df, bbox=bbox) +
    tm_polygons(polygon_col, 
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
