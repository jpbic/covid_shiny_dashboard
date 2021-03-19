required_packages = c(
  'dplyr',
  'tmap',
  'tmaptools',
  'lubridate',
  'gifski',
  'tidyr',
  'slider',
  'sf',
  'RColorBrewer',
  'usmap'
)
lapply(required_packages, require, character.only = T)

source('./utils/config/mp4_config.R')

conf_pal = c('#dbe4bc', '#f4f2f0', '#f4dac4', '#f4c7a6', '#e08b60')
pop_pal = c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603")
conf_breaks = c(-Inf, -3, 3, 10, 25, Inf)
pop_breaks = c(0, 0.5, 2, 5, 10, Inf)
us_bbox = bb(c(-140, 25, -65, 50), current.projection = 4326, projection=4326)
data(statepop)

generate_mp4 = function(
  input_file, 
  output_file, 
  type, 
  group_by_cols,
  geom_join_df,
  geom_left_join_by,
  geom_right_join_by,
  pop_df=NA,
  pop_left_join_by,
  pop_right_join_by,
  pop_col,
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
                          geom_left_join_by, geom_right_join_by, pop_df, pop_left_join_by,
                          pop_right_join_by, pop_col)
  
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
  pop_df=NA,
  pop_left_join_by,
  pop_right_join_by,
  pop_col
) {
  message('Creating SF Object')
  
  group_by_cols = append(group_by_cols, 'date')
  join_vector = setNames(right_join_by, left_join_by)
  
  sf_df = read.csv(input_file, stringsAsFactors = T) %>%
    filter(type == type) %>%
    pivot_longer(starts_with('X'), names_to = 'date', values_to = 'data_col') %>%
    mutate(date = as.Date(gsub('X', '', date), '%m.%d.%y')) %>%
    group_by(!!!syms(group_by_cols)) %>%
    summarise(data_col = sum(data_col)) %>%
    mutate(change = data_col - lag(data_col, n = 1, default=0, order_by=!!!syms(group_by_cols))) %>%
    mutate(rolling_ave_change = slide_dbl(change, mean, .before=7, .after=0)) %>%
    mutate(change_delta = change - lag(change, n=1, default=0, order_by=!!!syms(group_by_cols))) %>%
    mutate(rolling_ave_change_delta = slide_dbl(change_delta, mean, .before=7, .after=0)) %>%
    inner_join(join_df, by=join_vector) %>%
    ungroup()
  
  if (!is.na(pop_df)) {
    join_vector = setNames(pop_right_join_by, pop_left_join_by)
    sf_df = left_join(sf_df, pop_df, by=join_vector) %>%
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
        pmin(pop_perc, perc_change) >= 9.25 &
          pmin(lag(pop_perc, n=1, default=0, order_by=!!!syms(group_by_cols)), 
              lag(perc_change, n=1, default=0, order_by=!!!syms(group_by_cols))) < 9.25,
        'outbreak',
        ifelse(
          pmax(pop_perc, perc_change) < 9.25 &
            pmax(lag(perc_change, n=1, default=0, order_by=!!!syms(group_by_cols)),
                lag(pop_perc, n=1, default=0, order_by=!!!syms(group_by_cols))) >= 9.25,
          'stable',
          NA
        )
      ))
    
    sf_df$status[1] = 'stable'
    for (i in 2:nrow(sf_df)) {
      if(is.na(sf_df$status[i])) {
        sf_df$status[i] = sf_df$status[i-1]
      }
    }
    sf_df = mutate(sf_df, status_bin = ifelse(status=='outbreak', 1, -1)) %>%
      mutate(status = factor(status))
  }
  
  message('SF Object Created')
  return(st_sf(sf_df))
}

get_maps_mp4 = function(sf_df, legend_title, polygon_col, breaks=NULL, pal=NULL, bbox=NULL) {
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
    tm_symbols(size = 0.75, col="status_bin", shape=21, breaks=c(-1, 0, 1), 
               palette=c('#91cf60', '#fc8d59'), border.col='white', title.col='Status',
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

#do.call(generate_mp4, us_config)
