required_packages = c(
  'USAboundaries',
  'USAboundariesData',
  'usmap'
)
lapply(required_packages, require, character.only=T)

data(statepop)

us_config = list(
  input_file='./data/us_data.csv', 
  output_file='./www/us_anim.mp4', 
  type='confirmed', 
  group_by_cols='Province_State',
  geom_join_df=select(USAboundaries::us_states(), name, geometry),
  geom_left_join_by='Province_State',
  geom_right_join_by='name',
  pop_df=select(statepop, full, pop_2015),
  pop_left_join_by='Province_State',
  pop_right_join_by='full',
  pop_col='pop_2015',
  legend_title='Normalized Population %', 
  polygon_col='pop_perc', 
  breaks=c(0, 1.5, 5, 10, 15, Inf), 
  pal=c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603"), 
  bbox=bb(c(-140, 25, -65, 50), current.projection = 4326, projection=4326),
  width=1044,
  height=470,
  fps=8
)
