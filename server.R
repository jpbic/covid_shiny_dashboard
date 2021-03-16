required_packages = c(
  'dplyr',
  'tmap',
  'tmaptools',
  'sf',
  'ggplot2',
  'tidyr',
  'lubridate',
  'countrycode',
  'sp',
  'shinyjs'
)
lapply(required_packages, require, character.only = T)

shinyServer(function(input, output, session) {
  observeEvent(input$time_lapse_play_button, {
    js$play_video('time_lapse_vid')
  })
  
  observeEvent(input$time_lapse_status, {
    updateActionButton(
      session,
      'time_lapse_play_button',
      icon=icon(ifelse(input$time_lapse_status == 'paused', 
                       'play', 
                       ifelse(input$time_lapse_status == 'playing', 'pause', 'undo-alt')),
                class='fa-xs'
                )
    )
  })
})