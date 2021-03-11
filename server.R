required_packages = c(
  'dplyr',
  'tmap',
  'tmaptools',
  'sf',
  'ggplot2',
  'tidyr',
  'lubridate',
  'countrycode',
  'sp'
)
lapply(required_packages, require, character.only = T)

shinyServer(function(input, output, session) {

})