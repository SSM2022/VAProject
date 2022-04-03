#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyquant)
library(plotly)
library(reactable)
library(shinybusy)
library(lubridate)
library(xts)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)




ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tagList(
      tags$span(
        class = "logo-mini", "SSA"
      ),
      tags$span(
        class = "logo-lg", "Sugar Future Price Analyzer"
      )
    ),
    titleWidth = 250,
    tags$li(class = "dropdown", 
            tags$a(href = "https://isss608stock.netlify.app/#guide", 
                   target="_blank",
                   tags$i(class = "fas fa-question-circle"),
                   tags$span("Help"))
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore", tabName = "explore", icon = icon("chart-bar")),
      menuItem("Technical Analysis", tabName = "TA", icon = icon("chart-bar")),
      menuItem("Forecast", tabName = "forecast", icon = icon("dice-d20"))
    )
  ),
  dashboardBody(
   
    
    tags$head(tags$style(HTML('
          .nav-tabs-custom {
            margin-bottom: 0px;
            box-shadow: none;
          }
        '))),
    
    tabItems(
      # Explore tab content
      tabItem(tabName = "explore",
              exploreUI("explore")
      ),
      
      # TA tab content
      tabItem(tabName = "TA",
              TAUI("TA")
      ),
      
      # forecast tab content
      tabItem(tabName = "forecast",
              forecastUI("forecast")
      )

    )
  )
)

server <- function(input, output) {
  exploreServer("explore")
  TAServer("TA")
  forecastServer("forecast")
}

shinyApp(ui, server)