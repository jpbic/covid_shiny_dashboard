required_packages = c(
  'USAboundaries',
  'USAboundariesData',
  'tmap'
)
lapply(required_packages, require, character.only=T)

data(World)

us_config = list(
  input_file='./data/us_data.csv', 
  output_file='./www/us_anim.mp4', 
  type='confirmed', 
  geom_join_df=select(us_states(), name, geometry),
  geom_left_join_by='Province_State',
  geom_right_join_by='name',
  continent='none',
  legend_title='Normalized Population %', 
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
  geom_join_df=select(World, iso_a3, geometry),
  geom_left_join_by='iso3',
  geom_right_join_by='iso_a3',
  continent='none',
  legend_title='Normalized Population %', 
  breaks=c(0, 1.5, 5, 10, 15, Inf), 
  pal=c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603"), 
  bbox=NULL,
  width=1044,
  height=470,
  fps=8
)

europe_config = list(
  input_file='./data/all_data_with_iso.csv', 
  output_file='./www/europe_anim.mp4', 
  type='confirmed', 
  geom_join_df=select(World, iso_a3, geometry),
  geom_left_join_by='iso3',
  geom_right_join_by='iso_a3',
  continent='Europe',
  legend_title='Normalized Population %', 
  breaks=c(0, 1.5, 5, 10, 15, Inf), 
  pal=c("#fffcfa", "#FDBE85", "#FD8D3C", "#E6550D","#A63603"), 
  bbox=bb(c(-40, 32, 40, 72), current.projection = 4326, projection=4326),
  width=1044,
  height=470,
  fps=8
)
