# required_packages = c(
#   'dplyr',
#   'tmap',
#   'tmaptools',
#   'sf',
#   'ggplot2',
#   'tidyr',
#   'lubridate',
#   'countrycode',
#   'sp',
#   'shinyjs'
# )
# lapply(required_packages, require, character.only = T)
library(dplyr)
library(tmap)
library(tmaptools)
library(sf)
library(ggplot2)
library(tidyr)
library(lubridate)
library(countrycode)
library(sp)
library(shinyjs)

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
      sort(unique(us_graph_data$list_country))
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
      global_graph_data 
    } else {
      us_graph_data
    }
  })
  
  observeEvent(req(input$state_province_select_outbreak, cancelOutput=T), {
    df = outbreak_graph_data()
    prov_df = df %>%
      filter(list_country==input$state_province_select_outbreak)
    output$outbreak_plot = renderPlot(
      prov_df  %>%
        bind_rows(
          filter(., status_bin==-1) %>% mutate(status_bin=1, status='outbreak', rolling_ave_confirmed=0),
          filter(., status_bin==1) %>% mutate(status_bin=-1, status='stable', rolling_ave_confirmed=0)
        ) %>%
        mutate(status=factor(status), status_bin=factor(status_bin)) %>%
        ggplot(aes(x=date, y=rolling_ave_confirmed, fill=status_bin)) +
        geom_area() +
        ggtitle('Outbreaks') + ylab('7D Rolling Ave New Cases') +
        scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
        scale_fill_manual(name='Status', values=c('#073663', '#FD8D3C'), labels=c('Stable', 'Outbreak')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ylim(0, prov_df$population[1]*0.002)
    )
    output$outbreak_days_box = renderValueBox({
      valueBox(
        'Total',
        HTML(paste0('Outbreak Days: ', 
                    length(prov_df$status[prov_df$status == 'outbreak']))),
        icon=icon('virus'),
        color='aqua'
      )
    })
    df = df %>% group_by(list_country) %>%
      summarise(ob_days = sum(ifelse(status == 'outbreak', 1, 0))) %>%
      mutate(days_percentile = trunc(rank(ob_days)) / length(ob_days)) %>%
      filter(list_country == input$state_province_select_outbreak)
    output$outbreak_percentile_box = renderValueBox({
      valueBox(
        'Percentile',
        HTML(paste0('Outbreak Days: ',
                    format(round(df$days_percentile*100, 0), nsmall=0, big.mark=","))),
        icon=icon('percent'),
        color='purple'
      )
    })
    output$outbreak_max_box = renderValueBox({
      valueBox(
        'Maximum',
        HTML(paste0('New Cases: ', 
                    format(round(max(prov_df$rolling_ave_confirmed), 0), nsmall=0, big.mark=","))),
        icon=icon('chevron-up'),
        color='aqua'
      )
    })
  })
  
  # Metrics ####
  state_province_metric = reactive({
    if (input$metric_graph_level == 'us') {
      sort(unique(us_graph_data$list_country))
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
      global_graph_data
    } else {
      us_graph_data
    }
  })
  
  observeEvent({
    req(input$state_province_select_metric, cancelOutput=T)
    input$metric_set
  }, {
    df = metric_graph_data()
    prov_df = df %>% filter(list_country == input$state_province_select_metric)
    if (input$metric_set == 'Cumulative') {
      output$metrics = renderPlot(
        prov_df %>%
          pivot_longer(c(confirmed, deaths), names_to='type', values_to='values') %>%
          ggplot(aes(x=date, y=values, fill=type)) + 
          geom_line(position='stack', size=0.65) +
          geom_area(alpha=0.55, position='stack') +
          ggtitle('Cumulative') + ylab('Cumulative Confirmed/Deaths') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          scale_fill_manual(name='Type', values=c('#073663', '#FD8D3C'), labels=c('Confirmed', 'Deaths')) +
          theme(plot.title=element_text(hjust=0.5))
      )
      output$metric_ave_box = renderValueBox({
        valueBox(
          'Averages',
          HTML(paste0('Confirmed Cases: ', 
                      format(round(mean(prov_df$confirmed), 0), nsmall=0, big.mark=","), 
                      br(), 
                      'Deaths: ', 
                      format(round(mean(prov_df$deaths), 0), nsmall=0, big.mark=","))),
          icon=icon('arrows-alt-h'),
          color='aqua'
        )
      })
      df = df %>% group_by(list_country) %>% 
        summarise(ave_conf = mean(confirmed / (population * .002)), 
                  ave_deaths = mean(deaths / (population * .002))) %>%
        mutate(conf_percentile = trunc(rank(ave_conf)) / length(ave_conf), 
               death_percentile = trunc(rank(ave_deaths)) / length(ave_deaths)) %>%
        filter(list_country == input$state_province_select_metric)
      output$metric_percentile_box = renderValueBox({
        valueBox(
          'Percentiles',
          HTML(paste0('Confirmed Cases (Pop Adj): ',
                      format(round(df$conf_percentile*100, 0), nsmall=0, big.mark=","), 
                      br(),
                      'Deaths (Pop Adj): ',
                      format(round(df$death_percentile*100, 0), nsmall=0, big.mark=","))),
          icon=icon('percent'),
          color='purple'
        )
      })
      output$metric_max_box = renderValueBox({
        valueBox(
          'Maxima',
          HTML(paste0('Confirmed Cases: ', 
                      format(round(max(prov_df$confirmed), 0), nsmall=0, big.mark=","), 
                      br(), 
                      'Deaths: ', 
                      format(round(max(prov_df$deaths), 0), nsmall=0, big.mark=","))),
          icon=icon('chevron-up'),
          color='aqua'
        )
      })
    } else if (input$metric_set == 'Rolling Average Change') {
      output$metrics = renderPlot(
        prov_df %>%
          pivot_longer(c(rolling_ave_confirmed, rolling_ave_deaths), names_to='type', values_to='values') %>%
          ggplot(aes(x=date, y=values, fill=type)) + 
          geom_line(position='stack', size=0.65) +
          geom_area(alpha=0.55, position='stack') +
          ggtitle('7D Rolling Average') + ylab('7D Rolling Ave Confirmed/Deaths') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          scale_fill_manual(name='Type', values=c('#073663', '#FD8D3C'), labels=c('Confirmed', 'Deaths')) +
          theme(plot.title=element_text(hjust=0.5))
      )
      output$metric_ave_box = renderValueBox({
        valueBox(
          'Averages',
          HTML(paste0('Ave Daily New Cases: ', 
                      format(round(mean(prov_df$rolling_ave_confirmed), 0), nsmall=0, big.mark=","), 
                      br(), 
                      'Ave Daily Deaths: ', 
                      format(round(mean(prov_df$rolling_ave_deaths), 0), nsmall=0, big.mark=","))),
          icon=icon('arrows-alt-h'),
          color='aqua'
        )
      })
      df = df %>% group_by(list_country) %>% 
        summarise(ave_conf = mean(rolling_ave_confirmed / (population * .002)), 
                  ave_deaths = mean(rolling_ave_deaths / (population * .002))) %>%
        mutate(conf_percentile = trunc(rank(ave_conf)) / length(ave_conf), 
               death_percentile = trunc(rank(ave_deaths)) / length(ave_deaths)) %>%
        filter(list_country == input$state_province_select_metric)
      output$metric_percentile_box = renderValueBox({
        valueBox(
          'Percentiles',
          HTML(paste0('Ave Daily New Cases (Pop Adj): ',
                      format(round(df$conf_percentile*100, 0), nsmall=0, big.mark=","), 
                      br(),
                      'Ave Daily Deaths: ',
                      format(round(df$death_percentile*100, 0), nsmall=0, big.mark=","))),
          icon=icon('percent'),
          color='purple'
        )
      })
      output$metric_max_box = renderValueBox({
        valueBox(
          'Maxima',
          HTML(paste0('Ave Daily New Cases: ', 
                      format(round(max(prov_df$rolling_ave_confirmed), 0), nsmall=0, big.mark=","), 
                      br(), 
                      'Ave Daily Deaths: ', 
                      format(round(max(prov_df$rolling_ave_deaths), 0), nsmall=0, big.mark=","))),
          icon=icon('chevron-up'),
          color='aqua'
        )
      })
    } else {
      output$metrics = renderPlot(
        prov_df %>%
          ggplot(aes(x=date, y=mortality_rate*100)) + 
          geom_area(fill='#FD8D3C', alpha=0.55) +
          geom_line(color='#FD8D3C') +
          ggtitle('Mortality Rate') + ylab('Mortality Rate (%)') +
          scale_x_date('Date', date_breaks='1 month', date_labels='%b-%y') +
          ylim(0, 35) +
          theme(plot.title=element_text(hjust=0.5))
      )
      output$metric_ave_box = renderValueBox({
        valueBox(
          'Average',
          HTML(paste0('Mortality Rate: ', 
                      format(round(mean(prov_df$mortality_rate*100), 0), nsmall=0, big.mark=","),
                      '%')),
          icon=icon('arrows-alt-h'),
          color='aqua'
        )
      })
      df = df %>% group_by(list_country) %>% 
        summarise(ave_mortality = mean(mortality_rate)) %>%
        mutate(mortality_percentile = trunc(rank(ave_mortality)) / length(ave_mortality)) %>%
        filter(list_country == input$state_province_select_metric)
      output$metric_percentile_box = renderValueBox({
        valueBox(
          'Percentile',
          HTML(paste0('Mortality Rate: ',
                      format(round(df$mortality_percentile*100, 0), nsmall=0, big.mark=","))),
          icon=icon('percent'),
          color='purple'
        )
      })
      output$metric_max_box = renderValueBox({
        valueBox(
          'Maximum',
          HTML(paste0('Mortality Rate: ', 
                      format(round(max(prov_df$mortality_rate*100), 0), nsmall=0, big.mark=","),
                      '%')),
          icon=icon('chevron-up'),
          color='aqua'
        )
      })
    }
  })
})