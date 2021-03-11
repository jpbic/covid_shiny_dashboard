library(shiny)
library(shinydashboard)

# Header ####
header = dashboardHeader(title = 'COVID-19 Analysis')

# Sidebar ####
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem(
      'Time Lapse',
      tabName = 'time_lapse',
      icon = icon('globe-americas')
    )
  )
)

# Body ####
body = dashboardBody(
  tags$head(tags$style(HTML('
    video:focus {
      outline: none;
    }
    
    .controls {
      border: none;
      outline: none;
    }
  '))),
  tabItems(
    tabItem(
      tabName = 'time_lapse',
      box(
        width = 12,
        align = 'center',
        tags$video(
          'global_confirmed', 
          type='video/mp4', 
          src='conf_anim.mp4',
          controls='controls'
        )
      )
    )
  )
)

shinyUI(dashboardPage(header, sidebar, body))