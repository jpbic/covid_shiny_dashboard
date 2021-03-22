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
  
  # Time Lapse ####
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
  
  # Outbreak Analysis ####
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
      'state_province_select_outbreak',
      label=ifelse(input$outbreak_graph_level == 'us', 'State', 'Country'),
      choices=state_province_choices()
    )
  })
  
  outbreak_graph_data = eventReactive(input$state_province_select_outbreak, {
    g_level = ifelse(
      is.null(input$outbreak_graph_level), 
      initial_outbreak_graph_level, 
      input$outbreak_graph_level
    )
    if (g_level == 'global') {
      global_graph_data %>%
        filter(list_country==input$state_province_select_outbreak) %>%
        bind_rows(
          filter(., status_bin==-1) %>% mutate(status_bin=1, status='outbreak', rolling_ave_confirmed=0),
          filter(., status_bin==1) %>% mutate(status_bin=-1, status='stable', rolling_ave_confirmed=0)
        )
    } else {
      us_graph_data %>%
        filter(Province_State==input$state_province_select_outbreak) %>%
        bind_rows(
          filter(., status_bin==-1) %>% mutate(status_bin=1, status='outbreak', rolling_ave_confirmed=0),
          filter(., status_bin==1) %>% mutate(status_bin=-1, status='stable', rolling_ave_confirmed=0)
        )
    }
  })
  
  observeEvent(req(input$state_province_select_outbreak, cancelOutput=T), {
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
  
  # Metrics ####
  state_province_metric = reactive({
    if (input$metric_graph_level == 'us') {
      sort(unique(us_graph_data$Province_State))
    } else {
      sort(unique(global_graph_data$list_country))
    }
  })
  
  observeEvent(input$metric_graph_level, {
    updateSelectInput(
      session,
      'state_province_select_metric',
      label=ifelse(input$metric_graph_level == 'us', 'State', 'Country'),
      choices=state_province_metric()
    )
  })
  
  metric_graph_data = eventReactive(input$state_province_select_metric, {
    g_level = ifelse(
      is.null(input$metric_graph_level), 
      initial_outbreak_graph_level, 
      input$metric_graph_level
    )
    if (g_level == 'global') {
      global_graph_data %>% filter(list_country == input$state_province_select_metric)
    } else {
      us_graph_data %>% filter(Province_State == input$state_province_select_metric)
    }
  })
  
  observeEvent({
    req(input$state_province_select_metric, cancelOutput=T)
    input$metric_set
  }, {
    df = metric_graph_data()
    if (input$metric_set == 'Cumulative') {
      output$metrics = renderPlot(
        df %>%
          pivot_longer(c(confirmed, deaths), names_to='type', values_to='values') %>%
          ggplot(aes(x=date, y=values, fill=type)) + 
          geom_line(position='stack', size=0.65) +
          geom_area(alpha=0.55, position='stack') +
          ggtitle('Cumulative') + ylab('Cumulative Confirmed/Deaths') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          scale_fill_manual(name='Type', values=c('#073663', '#FD8D3C'), labels=c('Confirmed', 'Deaths')) +
          theme(plot.title=element_text(hjust=0.5))
      )
    } else if (input$metric_set == 'Rolling Average Change') {
      output$metrics = renderPlot(
        df %>%
          pivot_longer(c(rolling_ave_confirmed, rolling_ave_deaths), names_to='type', values_to='values') %>%
          ggplot(aes(x=date, y=values, fill=type)) + 
          geom_line(position='stack', size=0.65) +
          geom_area(alpha=0.55, position='stack') +
          ggtitle('7D Rolling Average') + ylab('7D Rolling Ave Confirmed/Deaths') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          scale_fill_manual(name='Type', values=c('#073663', '#FD8D3C'), labels=c('Confirmed', 'Deaths')) +
          theme(plot.title=element_text(hjust=0.5))
      )
    } else {
      output$metrics = renderPlot(
        df %>%
          ggplot(aes(x=date, y=mortality_rate*100)) + 
          geom_area(fill='#FD8D3C', alpha=0.55) +
          geom_line(color='#FD8D3C') +
          ggtitle('Mortality Rate') + ylab('Mortality Rate (%)') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          ylim(0, 35) +
          theme(plot.title=element_text(hjust=0.5))
      )
    }
  })
})