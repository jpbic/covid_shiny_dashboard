library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(jsonlite)
source('./www/css/custom_theme.R')
source('./www/about_content.R')

# Header ####
header = dashboardHeader(title = 'COVID-19 Analysis', 
                         tags$li(class='dropdown',
                                 tags$a(
                                   class='fab fa-linkedin 3x',
                                   href='https://www.linkedin.com/in/jason-polek-2b248037/',
                                   target='_blank'
                                 )
                         ))

# Sidebar ####
sidebar = dashboardSidebar(
  collapsed=T,
  sidebarMenu(
    id='tabs',
    menuItem(
      'Time Lapse',
      tabName = 'time_lapse',
      icon = icon('stopwatch')
    ),
    menuItem(
      'Graphing',
      icon = icon('chart-area'),
      menuSubItem(
        'Outbreak Analysis',
        tabName = 'outbreak_charts',
        icon = NULL
      ),
      menuSubItem(
        'Metrics',
        tabName = 'metrics',
        icon = NULL
      )
    ),
    menuItem(
      'About',
      tabName = 'about',
      icon = icon('info-circle')
    )
  )
)

# Body ####
body = dashboardBody(
  theme_custom,
  useShinyjs(),
  tags$head(tags$link(rel='stylesheet', type='text/css', href='css/styles.css')),
  extendShinyjs(
    script='js/script.js', 
    functions=c('play_video', 'set_outbreak_level', 'set_metric_level')
  ),
  tabItems(
    
    #  > Time Lapse ####
    tabItem(
      tabName = 'time_lapse',
      absolutePanel(
        width='100%',
        div(
          tags$video(
            'time_lapse_vid',
            id='time_lapse_vid',
            type='video/mp4',
            src='global_anim.mp4',
            autoplay=NA,
            muted=NA,
            width='100%',
            height='auto'
          ),
          
          # >> Video Controls ####
          absolutePanel(
            id='video-controls-wrapper',
            class='video-controls',
            div(
              circleButton(
                'time_lapse_play_button',
                icon=icon('pause', class='fa-xs')
              )
            ),
            
            # >>> Slider ####
            div(
              class='slider-wrapper',
              div(
                class='time-wrapper',
                id='seek-tooltip',
                textOutput('time_elapsed')
              ),
              div(
                class='video-progress',
                tags$progress(
                  id='progress-bar',
                  value='0',
                  min='0'
                ),
                tags$input(
                  class='seek',
                  id='seek',
                  value='0',
                  min='0',
                  type='range',
                  step='1'
                )
              ),
              div(
                class='time-wrapper',
                id='duration-wrapper',
                textOutput('duration')
              )
            )
          )
        ),
        
        # >> Time Lapse Choices ####
        absolutePanel(
          id='time_lapse_radio-button-wrapper',
          class='radio-button-wrapper',
          div(
            id='time-lapse-choice-list',
            class='radio-button-choice-list',
            tags$h4('Choose Time Lapse', id='tl_title'),
            tags$label(
              class='radio-container',
              'Global Outbreak Time Lapse',
              tags$input(
                type='radio',
                name='time-lapse',
                value='global_anim.mp4',
                checked='checked'
              ),
              tags$span(
                class='checkmark'
              )
            ),
            tags$label(
              class='radio-container',
              'US Outbreak Time Lapse',
              tags$input(
                type='radio',
                name='time-lapse',
                value='us_anim.mp4'
              ),
              tags$span(
                class='checkmark'
              )
            ),
            tags$label(
              class='radio-container',
              'Europe Outbreak Time Lapse',
              tags$input(
                type='radio',
                name='time-lapse',
                value='europe_anim.mp4'
              ),
              tags$span(
                class='checkmark'
              )
            )
          )
        )
      )
    ),
    
    # > Outbreak Plots ####
    tabItem(
      tabName='outbreak_charts',
      box(
        plotOutput('outbreak_plot'),
        width=8,
        height='100%'
      ),
      box(
        id='outbreak-controls-box',
        width=4,
        height='100%',
        div(
          id='ob_control_wrapper',
          tags$h2('Graph Controls', class='control_title')
        ),
        div(
          id='outbreak_graph_level',
          class='radio-button-choice-list graph-level',
          tags$h4('Graph Level'),
          tags$label(
            class='radio-container',
            'Global',
            tags$input(
              type='radio',
              name='outbreak_graph',
              value='global',
              checked='checked'
            ),
            tags$span(
              class='checkmark'
            )
          ),
          tags$label(
            class='radio-container',
            'US',
            tags$input(
              type='radio',
              name='outbreak_graph',
              value='us'
            ),
            tags$span(
              class='checkmark'
            )
          )
        ),
        selectInput(
          'state_province_select_outbreak',
          label='Country',
          choices=sort(unique(global_graph_data$list_country))
        )
      )
    ),
    
    # > Metrics ####
    tabItem(
      tabName='metrics',
      box(
        width=8,
        height='100%',
        plotOutput('metrics')
      ),
      box(
        id='metrics-controls-box',
        width=4,
        height='100%',
        div(
          id='metric_control_wrapper',
          tags$h2('Graph Controls', class='control_title')
        ), 
        div(
          id='metric_graph_level',
          class='radio-button-choice-list graph-level',
          tags$h4('Graph Level'),
          tags$label(
            class='radio-container',
            'Global',
            tags$input(
              type='radio',
              name='metric_graph',
              value='global',
              checked='checked'
            ),
            tags$span(
              class='checkmark'
            )
          ),
          tags$label(
            class='radio-container',
            'US',
            tags$input(
              type='radio',
              name='metric_graph',
              value='us'
            ),
            tags$span(
              class='checkmark'
            )
          )
        ),
        selectInput(
          'state_province_select_metric',
          label='Country',
          choices=sort(unique(global_graph_data$list_country))
        ),
        selectInput(
          'metric_set',
          label='Metric Set',
          choices=metric_set_choices
        )
      )
    ),
    
    # > About ####
    tabItem(
      tabName='about',
      absolutePanel(
        id='about-panel',
        width='100%',
        height='100%',
        div(
          id='about-container',
          tags$h1('Comparable Figures for COVID-19', id='about-title'),
          HTML(about_content)
        )
      )
    )
  )
)

shinyUI(dashboardPage(header, sidebar, body))