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

TAUI <- function(id){
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
        selectInput(inputId = NS(id,"Chart"),
                    label = "Chart Type",
                    choices = c(
                      "Candlestick" = "candlestick",
                      "Averages - Simple Moving Average" = "sma",
                      "Averages - Exponential Moving Average" = "ema",
                      "Averages - Double Exponential Moving Average" = "dema",
                      "Averages - Elastic volume-weighted moving averages" = "evwma",
                      "Simple Moving Average +Bollinger Bands" = "bollinger"),
                    selected = "Candlestick"),
        width = 3
      ),
      mainPanel(plotOutput(NS(id,"plot"))
      )
    )
  )
}


TAServer <- function(id) {
  moduleServer(id, function(input, output, session){
    output$plot<-renderPlot({
      
      if(input$Chart == "bollinger"){
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          
          ggplot(aes(x = date, y = close, open = open,
                     high = high, low = low, close = close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred",
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          geom_bbands(ma_fun = SMA, sd = 2, n = 20,show.legend = TRUE) +
          labs (title = "Bollinger Bands with SMA Applied",
                subtitle = "24 Weeks Candlestick Chart",
                y = "Closing Price", x = "")+
          scale_x_date(breaks = "4 weeks", date_labels =  "%Y-%m") +
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"),
            axis.text.x = element_text(size=10,angle = 0)
            
          )
      }
      
      else if(input$Chart == "candlestick"){        
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          ggplot(aes(x = date, y =close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred", 
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          labs(title = "Opening and Closing Prices",subtitle = "Candlestick Chart", y ="Price", x = "") +
          scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") +
          scale_color_tq(theme = "dark")+
          theme(plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill="light grey"),
                legend.text = element_text(colour="black"))+
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"))+
          theme(axis.text.x = element_text(size=10,angle = 0, hjust = 1))
      }
      
      
      else  if (input$Chart == "evwma"){
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          ggplot(aes(x = date, y = close, volume = volume )) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred", 
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          geom_ma(ma_fun = EVWMA, n = 20, wilder = TRUE, color  = "blue", size = 1.25) +
          geom_ma(ma_fun = EVWMA, n = 50, wilder = TRUE, color = "red", size = 1.25) + 
          labs(title = "Elastic, volume-weighted moving averages", 
               subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
               y = "Closing Price", x = "") +
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"))+
          scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") +
          coord_x_date(xlim = c(input$date_range[2] - 168,input$date_range[2]))+
          theme(axis.text.x = element_text(size=10,angle = 0, hjust = 1))
        
      }   
      else  if (input$Chart == "sma"){
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          ggplot(aes(x = date, y = close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred", 
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          geom_ma(ma_fun = SMA, n = 20, wilder = TRUE, color  = "blue", size = 1.25) +
          geom_ma(ma_fun = SMA, n = 50, wilder = TRUE, color = "red", size = 1.25) + 
          labs(title = "Simple Moving Average Chart", 
               subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
               y = "Closing Price", x = "") +
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"))+
          scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") +
          coord_x_date(xlim = c(input$date_range[2] - 168,input$date_range[2]))+
          theme(axis.text.x = element_text(size=10,angle = 0, hjust = 1))
        
      }   
      else  if (input$Chart == "dema"){
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          ggplot(aes(x = date, y = close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred", 
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          geom_ma(ma_fun = DEMA, n = 20, wilder = TRUE, color  = "blue", size = 1) +
          geom_ma(ma_fun = DEMA, n = 50, wilder = TRUE, color = "red", size = 1) + 
          labs(title = "Double-exponential moving average", 
               subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
               y = "Closing Price", x = "") +
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"))+
          scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") +
          coord_x_date(xlim = c(input$date_range[2] - 168,input$date_range[2]))+
          theme(axis.text.x = element_text(size=10,angle = 0, hjust = 1))
        
      }   
      
      else  if (input$Chart == "ema"){
        data %>% 
          filter(symbol == input$futures) %>%
          filter(date>= input$date_range[1] & date<=input$date_range[2])%>%
          ggplot(aes(x = date, y = close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close),
                           colour_up = "darkgreen", colour_down = "darkred", 
                           fill_up  = "darkgreen", fill_down  = "darkred")+
          geom_line()+
          geom_point(color="red")+
          geom_ma(aes(volume=volume),ma_fun = EMA, n = 20, wilder = TRUE, color  = "blue", size = 1) +
          geom_ma(aes(volume=volume),ma_fun = EMA, n = 50, wilder = TRUE, color = "red", size = 1) + 
          labs(title = "Exponetial Moving Average Chart", 
               subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
               y = "Closing Price", x = "") +
          theme_tq()+
          theme(
            plot.title = element_text(color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(color = "black",size = 14),
            plot.caption = element_text(color = "green", face = "italic"))+
          scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") +
          coord_x_date(xlim = c(input$date_range[2] - 168,input$date_range[2]))+
          theme(axis.text.x = element_text(size=10,angle = 0, hjust = 1))
        
      }     
      
    })
    
    
  }
  )
}





ui <- fluidPage(
  TAUI("TA")
)
server <- function(input, output, session) {
  TAServer("TA")
}
shinyApp(ui, server)


