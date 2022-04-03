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
library(plotly)
library(lubridate)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(dplyr)
library(reactable)
library(glmnet)


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

#data <- data[order(as.Date(data$date, format="%d/%m/%Y")),]
##############



forecastUI <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = NS(id,"futures"),
                    label = "Futures Type",
                    choices = c("NY 11" = "SB",
                                "NY 16" = "SF",
                                "LDN 5" = "W"),
                    selected = "W"),
        dateRangeInput(inputId = NS(id,"date_range"), 
                       label = "Date range:",
                       start = "2021-01-15",
                       end   = "2022-03-01"),
        sliderInput(inputId =NS(id,"proportion"), 
                    label = "Training - Validation Split",
                    min = 0.8,
                    max = 0.9,
                    value = 0.9,
                    step = 0.01),
        numericInput(inputId = NS(id,"horizon"), 
                     label ="Forecast Horizon (Days)", 
                     value = 30),
        width = 3
      ),
      mainPanel(
	  reactableOutput(NS(id,'forcastPdt')),
	  plotOutput(NS(id,"residualP")),
	  plotlyOutput(NS(id,"calibrationP")),
	  plotlyOutput(NS(id,"forcastP"))
        )
      )
    )
}


forecastServer <- function(id) {
  moduleServer(id, function(input, output, session){
  
  modata<-reactive({
 
   option<-data %>% 
      filter(symbol == input$futures) %>%
      filter(date> as_date(input$date_range[1]) & date<as_date(input$date_range[2])) %>%
	  select(date,close) %>%
	  arrange(date)
	  
	  
    splits <- initial_time_split(option, prop = input$proportion)
	

      #arima_boosted
      model_fit_arima_boosted <- arima_boost(
        min_n = 2,
        learn_rate = 0.015
      ) %>%
        set_engine(engine = "auto_arima_xgboost") %>%
        fit(close ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
            data = training(splits))
			
      model_fit_ets <- exp_smoothing() %>%
        set_engine(engine = "ets") %>%
        fit(close ~ date, data = training(splits))
      
      #prophet
      model_fit_prophet <- prophet_reg() %>%
        set_engine(engine = "prophet") %>%
        fit(close ~ date, data = training(splits))
      

      #GLMNET

      model_glmnet <- linear_reg(penalty = 0.01) %>%
        set_engine("glmnet") %>%
        fit(close ~ wday(date,label = TRUE)+ month(date,label = TRUE)+ as.numeric(date),
            data = training(splits))
      

      #Add fitted models to a Model Table.
      models_tbl <- modeltime_table(
        model_fit_arima_boosted,
        model_fit_ets,
        model_fit_prophet,
        model_glmnet
      )

#
#      #Calibrate the model to a testing set
      calibration_tbl <- models_tbl %>%
        modeltime_calibrate(new_data = testing(splits))
      

      ###Chart:calibration plots //try later to see if error
      #Testing Set Forecast & Accuracy Evaluation
      
      
      calibrationplots <- calibration_tbl %>%
      modeltime_forecast(
        new_data    = testing(splits),
        actual_data = option
      ) %>%
          plot_modeltime_forecast(
          .legend_max_width = 25, # For mobile screens
		  .title='Calibration Plot'
          
        )
      
      
      
      
      ###Chart:Residual plots
      residuals_tbl <- calibration_tbl %>%
        modeltime_residuals()


      residuals_plots <- residuals_tbl %>%
        plot_modeltime_residuals(.type = "timeplot",
                                 .facet_vars = .model_desc, 
                                 .facet_ncol = 2, 
                                 .interactive = FALSE,
                                 .facet_scales = "fixed") +
        labs(title = paste("Residuals Plot for", input$futures),
             y = "Residuals") +
        theme(panel.spacing = unit(0.25, "lines"),
              legend.position='none')
     
      

	  
      ###table:calibration plots
      #Testing Set Forecast & Accuracy Evaluation
     #########calibration_tbl %>%
     #########  modeltime_accuracy() %>%
     #########  table_modeltime_accuracy()
      #Refit to Full Dataset & Forecast Forward
      
      
      
      
     refit_tbl <- calibration_tbl %>%
       modeltime_refit(data = option)
     
     
     ## Chart: forecast  ###error!!!!!
     refit_tblplots <-
       refit_tbl %>%
       modeltime_forecast(h = sprintf("%s days", input$horizon), actual_data = option) %>%
       plot_modeltime_forecast(
         .legend_max_width = 25, # For mobile screens
       )
	   


  
      list(option,
	  splits,
	  model_fit_arima_boosted,
	  model_fit_ets,
	  model_fit_prophet,
	  model_glmnet,
	  models_tbl,
	  calibration_tbl,
	  calibrationplots,
	  residuals_tbl,
	  residuals_plots,
	  refit_tbl,
	  refit_tblplots
	  )
  })
  
  
  
  output$forcastPdt<-renderReactable({
  modata()->mo
  
   mo[[8]] %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(style = list(fontSize = "9px"))
  })
    

	
    output$residualP <- renderPlot({
	modata()->mo
	mo[[11]]
	})
	
    output$calibrationP <- renderPlotly({
	modata()->mo
	mo[[9]]
	})  

  
  output$forcastP<-renderPlotly({
	modata()->mo
	mo[[13]]
	})  

}
  )}


ui <- fluidPage(
  forecastUI("forecast")
)

server <- function(input, output, session) {
  forecastServer("forecast")
}



shinyApp(ui, server)





