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

initial_outbreak_graph_level = 'global'

shinyServer(function(input, output, session) {
  observeEvent(input$tabs, {
    toggleClass(
      selector='body',
      class='sidebar-collapse'
    )
  }, ignoreInit=T)
  
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
  
  state_province_choices = reactive({
    if (input$outbreak_graph_level == 'us') {
      sort(unique(us_graph_data$Province_State))
    } else {
      sort(unique(global_graph_data$list_country))
    }
  })
  
  observeEvent(input$outbreak_graph_level, {
    updateSelectInput(
      session,
      'state_province_select',
      label=ifelse(input$outbreak_graph_level == 'us', 'State', 'Country'),
      choices=state_province_choices()
    )
  })
  
  outbreak_graph_data = eventReactive(input$state_province_select, {
    g_level = ifelse(
      is.null(input$outbreak_graph_level), 
      initial_outbreak_graph_level, 
      input$outbreak_graph_level
    )
    if (g_level == 'global') {
      df = global_graph_data %>%
        filter(list_country==input$state_province_select) %>%
        bind_rows(
          filter(., status_bin==-1) %>% mutate(status_bin=1, status='outbreak', rolling_ave_confirmed=0),
          filter(., status_bin==1) %>% mutate(status_bin=-1, status='stable', rolling_ave_confirmed=0)
        )
    } else {
      df = us_graph_data %>%
        filter(Province_State==input$state_province_select) %>%
        bind_rows(
          filter(., status_bin==-1) %>% mutate(status_bin=1, status='outbreak', rolling_ave_confirmed=0),
          filter(., status_bin==1) %>% mutate(status_bin=-1, status='stable', rolling_ave_confirmed=0)
        )
    }
  })
  
  observeEvent(req(input$state_province_select, cancelOutput=T), {
    df = outbreak_graph_data()
    output$outbreak_plot = renderPlot(
      df %>%
        mutate(status=factor(status), status_bin=factor(status_bin)) %>%
        ggplot(aes(x=date, y=rolling_ave_confirmed, fill=status_bin)) +
        geom_area() +
        ggtitle('Outbreaks') + ylab('7D Rolling Ave New Cases') +
        scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
        scale_fill_manual(name='Status', values=c('#073663', '#FD8D3C'), labels=c('Stable', 'Outbreak')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ylim(0, df$population[1]*0.002)
    )
  })
})