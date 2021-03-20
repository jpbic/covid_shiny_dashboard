library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(jsonlite)
source('./www/css/custom_theme.R')

# Header ####
header = dashboardHeader(title = 'COVID-19 Analysis')

# Sidebar ####
sidebar = dashboardSidebar(
  collapsed=T,
  sidebarMenu(
    menuItem(
      'Time Lapse',
      tabName = 'time_lapse',
      icon = icon('stopwatch')
    )
  )
)

# Body ####
body = dashboardBody(
  theme_custom,
  useShinyjs(),
  tags$head(tags$link(rel='stylesheet', type='text/css', href='css/styles.css')),
  extendShinyjs(script='js/script.js', functions=c('play_video', 'video_state')),
  tabItems(
    
    # Time Lapse ####
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
          
          # > Video Controls ####
          absolutePanel(
            id='video-controls-wrapper',
            class='video-controls',
            div(
              circleButton(
                'time_lapse_play_button',
                icon=icon('pause', class='fa-xs')
              )
            ),
            
            # >> Slider ####
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
        
        # > Time Lapse Choices ####
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
            )
          )
        )
      )
    )
  )
)

shinyUI(dashboardPage(header, sidebar, body))