source('./utils/generate_gif_and_mp4.R')
source('./utils/config/mp4_config.R')

global_graph_data = do.call(create_map_data, global_graph_config)
us_graph_date = do.call(create_map_data, us_graph_config)
