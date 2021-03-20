required_packages = c(
  'USAboundaries',
  'USAboundariesData',
  'usmap',
  'tidyr'
)
lapply(required_packages, require, character.only=T)

data(statepop)
data(World)

us_config = list(
  input_file='./data/us_data.csv', 
  output_file='./www/us_anim.mp4', 
  type='confirmed', 
  group_by_cols=c('Province_State', 'date'),
  geom_join_df=select(us_states(), name, geometry),
  geom_left_join_by='Province_State',
  geom_right_join_by='name',
  pop_df=select(statepop, full, pop_2015),
  pop_left_join_by='Province_State',
  pop_right_join_by='full',
  pop_col='pop_2015',
  critical_value=9.25,
  legend_title='Normalized Population %', 
  polygon_col='pop_perc', 
  breaks=c(0, 1.5, 5, 10, 15, Inf), 
  pal=c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603"), 
  bbox=bb(c(-140, 25, -65, 50), current.projection = 4326, projection=4326),
  width=1044,
  height=470,
  fps=8
)

global_config = list(
  input_file='./data/all_data_with_iso.csv', 
  output_file='./www/global_anim.mp4', 
  type='confirmed', 
  group_by_cols=c('iso3', 'date'),
  geom_join_df=select(World, iso_a3, geometry),
  geom_left_join_by='iso3',
  geom_right_join_by='iso_a3',
  pop_df=select(filter(world_bank_pop, indicator=='SP.POP.TOTL'), country, `2017`),
  pop_left_join_by='iso3',
  pop_right_join_by='country',
  pop_col='2017',
  critical_value=2.5,
  legend_title='Normalized Population %', 
  polygon_col='pop_perc', 
  breaks=c(0, 1.5, 5, 10, 15, Inf), 
  pal=c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603"), 
  bbox=NULL,
  width=1044,
  height=470,
  fps=8
)
