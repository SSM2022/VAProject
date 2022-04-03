#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(timetk)
library(plotly)
library(tidyr)
library(tidymodels)
library(tidyquant)
library(modeltime)
library(lubridate)
library(forecast)

NY11 <- read_csv("data/NY 11.csv") %>%
  mutate('symbol' = 'SB' ) %>%
  rename('open' = '%SB 1!.Default Open',
         'high' = '%SB 1!.Default High',
         'low' = '%SB 1!.Default Low',
         'close' = '%SB 1!.Default Close',
         'openInterest' = 'OpenInterest.Default',
         'volume' = 'VOL (%SB 1!).Volume')

NY16 <- read_csv("data/NY 16.csv") %>%
  mutate('symbol' = 'SF' ) %>%
  rename('open' = '%SFQ 1!.Default Open',
         'high' = '%SFQ 1!.Default High',
         'low' = '%SFQ 1!.Default Low',
         'close' = '%SFQ 1!.Default Close',
         'openInterest' = 'OpenInterest.Default',
         'volume' = 'VOL (%SFQ 1!).Volume')

LDN5 <- read_csv("data/LDN 5.csv") %>%
  mutate('symbol' = 'W' ) %>%
  rename('open' = '%W 1!-ICE.Default Open',
         'high' = '%W 1!-ICE.Default High',
         'low' = '%W 1!-ICE.Default Low',
         'close' = '%W 1!-ICE.Default Close',
         'openInterest' = 'OpenInterest.Default',
         'volume' = 'VOL (%W 1!-ICE).Volume')

data <- rbind(NY11, NY16, LDN5)

data$date = mdy(data$Date)

data <- na.omit(data)


exploreUI <- function(id){
  ns <- NS(id)
  
  tagList(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = NS(id,"futures"),
                  label = "Futures Type",
                  choices = c("NY 11" = "SB",
                              "NY 16" = "SF",
                              "LDN 5" = "W"),
                  selected = "NY 11"),
      dateRangeInput(inputId = NS(id,"date_range"), 
                     label = "Date range:",
                     start = "2021-07-01",
                     end   = "2021-12-31"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(6,plotlyOutput(NS(id,"linePlot"), height = "400px", width = "100%")),
        column(6,plotlyOutput(NS(id,"Hist"), height = "400px", width = "100%"))
      ),
      fluidRow(
        column(6,plotlyOutput(NS(id,"Auto"), height = "400px", width = "100%")),
        column(6,plotlyOutput(NS(id,"Anomaly"), height = "400px", width = "100%"))
      )
          )
  )
  )
}

exploreServer <- function(id) {
  moduleServer(id, function(input, output, session){
    LP <- reactive({
    data %>% 
      filter(symbol == input$futures) %>%
      filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
      plot_time_series(date, close, .interactive = FALSE)+
      labs(title = "Daily Closing Price",
           y = "Price", x = "Date")+
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      theme(plot.title = element_text(face = 'bold', size = 16))
    })
    output$linePlot <- renderPlotly({ggplotly(LP())})
  

    HT <- reactive({
    data %>% 
      filter(symbol == input$futures) %>%
      filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
      ggplot(aes(x = date, y = volume)) +
      geom_segment(aes(xend = date, yend = 0)) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = "Daily Trading Volume",
           y = "Volume", x = "Date") + 
      scale_color_gradient(low = "red", high = "darkblue") +
      theme_tq() + 
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      theme(plot.title = element_text(face = 'bold', size = 16))
  })
   output$Hist <- renderPlotly({ggplotly(HT())})
      
      
   AT <- reactive({
    data %>% 
      filter(symbol == input$futures) %>%
      filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
      plot_stl_diagnostics(
        date, close,
        .frequency = "auto", .trend = "auto",
        .feature_set = c("season", "trend", "remainder"),
        .interactive = FALSE) +
      labs(title = "Seasonal-Trend Decomposition",
           y = "Price", x = "Date")+
      theme_tq() + 
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      theme(plot.title = element_text(face = 'bold', size = 16))
  })
  output$Auto <- renderPlotly({ggplotly(AT())})
  
    AM <- reactive({
    data %>% 
      filter(symbol == input$futures) %>%
      filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
      plot_anomaly_diagnostics(date, close,
                               .message = FALSE,
                               .facet_ncol = 2,
                               .ribbon_alpha = 0.25,
                               .interactive = FALSE,
                               .legend_show = FALSE) +
      labs(title = "Anomaly Diagnostics",
           y = "Price", x = "Date")+
      theme_tq() + 
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      theme(plot.title = element_text(face = 'bold', size = 16))
  })
    output$Anomaly <- renderPlotly({ggplotly(AM())})
  }
)
}

ui <- fluidPage(
  exploreUI("explore")
)
server <- function(input, output, session) {
  exploreServer("explore")
}
shinyApp(ui, server)
 
  
  
