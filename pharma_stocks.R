# App Description ---------------------------------------------------------
# App provides time series analysis of Pfizer, Johnson&Johnson, and Novartis 
# stock data from 5/1/2012-5/1/2017. App allows comparison of Open, 
# High, Low, Close, and Adj.Close prices along
# with time series modeling using Holt-Winters or ARIMA. Additionally, 
# evaluation of predictions (for 2017) using only 2012-2016 data 
# for model building is available. 
# Prep --------------------------------------------------------------------
library(MTS)
library(TTR)
library(forecast)
library(fma)
library(rsconnect)
library(shinythemes)
# Read In Data ------------------------------------------------------------
pfe <- read.csv('pfe_monthly.csv', stringsAsFactors = F)
jnj <- read.csv('jnj_monthly.csv', stringsAsFactors = F)
nvs <- read.csv('nvs_monthly.csv', stringsAsFactors = F)
pfe_ts <- msts(pfe[,2:6], seasonal.periods = 12)
jnj_ts <- msts(jnj[,2:6],  seasonal.periods = 12)
nvs_ts <- msts(nvs[,2:6],  seasonal.periods = 12)
# UI ----------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Pharmaceutical Stock Prices"),
  sidebarLayout(
  sidebarPanel(
    radioButtons('co', 'Company: ', 
                 c('Pfizer', 'Johnson&Johnson', 
                   'Novartis'), selected = 'Pfizer'), 
    radioButtons('metric', 'Metric: ', 
                 c('Open', 'High', 'Low', 'Close', 
                   'Adj.Close'), selected = c('Open')), 
    radioButtons('model', 'Model: ', 
                 c('Holt-Winters', 'ARIMA'), selected = c('ARIMA'))
  ), 
  mainPanel(
    tabsetPanel(
      tabPanel('Time Series Plot', plotOutput('ts_plot')),
      tabPanel('Stock Price Summaries', verbatimTextOutput('summary')),
      tabPanel('Time Series Decomposition', plotOutput('decomp')),
      tabPanel('Time Series Modeling', plotOutput('ts_models')), 
      tabPanel('Time Series Prediction Evaluation', verbatimTextOutput('evaluation'))
    )
  )
)))
# Server ------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  df <- reactive(switch(input$co, 'Pfizer' = pfe_ts, 'Johnson&Johnson' = jnj_ts, 
                        'Novartis' = nvs_ts))
  m <- reactive(switch(input$metric,
                       'Open' = 1, 'High' = 2, 'Low' = 3, 'Close' = 4, 
                       'Adj.Close' = 5))
  d <- reactive(switch(input$model, 'Holt-Winters' = 'hw', 'ARIMA' = 'arima'))
  output$ts_plot <- renderPlot({
    plot(df()[,1], ylab = 'Price in USD', xlab = 'Time Period (2012-2017)')
    lines(df()[,2], col = 2)
    lines(df()[,3], col = 3)
    lines(df()[,4], col = 4)
    lines(df()[,5], col = 5)
    legend('bottomright', c('Open', 'High', 'Low', 'Close', 'Adj.Close'), 
           col = c(1,2,3,4,5), lty = c(1,1,1,1,1))
  }) 
  output$summary <- renderPrint({
    summary(df())
  }) 
  output$decomp <- renderPlot({
    plot(decompose(df()[,m()]))
  })
  output$ts_models <-renderPlot({
    hw_forecast <- function(df, metric){
      par(mfrow=c(1,2))
      wind <- window(df[,metric], start = c(1), end = c(5))
      hw_fit <- HoltWinters(wind)
      plot(hw_fit, xlab = 'Year', main = ' ')
      legend('bottomright', c('Observed', 'Fitted'), col = c(1,2), lty = c(1,1))
      hw_pfe_fc <- forecast(hw_fit, h = 12)
      plot(hw_pfe_fc, main = ' ')
      pred_wind <- window(df[,metric], start = c(5), end = c(6))
      lines(pred_wind)
      return(list(as.numeric(hw_pfe_fc$mean), pred_wind[2:13]))
    }
    arima_forecast <- function(df, metric){
      par(mfrow=c(1,2))
      wind <- window(df[,metric], start = c(1), end = c(5))
      a_fit <- auto.arima(wind)
      plot(a_fit$x,ylab = 'Observed / Fitted', xlab = 'Year')
      lines(a_fit$fitted, col = 2)
      legend('bottomright', c('Observed', 'Fitted'), col = c(1,2), lty = c(1,1))
      a_fc <- forecast(a_fit, h = 12)
      plot(a_fc, main = ' ')
      pred_wind <- window(df[,metric], start = c(5), end = c(6))
      lines(pred_wind)
      return(list(as.numeric(a_fc$mean), pred_wind[2:13]))
    }
    if (d() == 'hw'){
      hw_forecast(df(), m())
    }
    else{
      arima_forecast(df(), m())  
    }
    output$evaluation <- renderPrint({
      mse <- function(val1, val2){
        n = length(val1)
        error <- (1/n)*sum((val1-val2)^2)
        cat(sprintf('The MSE is %s.', round(error, 2), quote = FALSE))
      }
      create_df <- function(val1, val2){
        tmp_df <- data.frame(val1, val2)
        tmp_df$Difference <- val1-val2
        colnames(tmp_df) <- c('Predicted', 'Actual', 'Difference')
        cat('\n')
        cat('\n')
        cat(' ------------ Time Series Dataframe ---------------- ')
        cat('\n')
        cat('\n')
        print(tmp_df)
      }
      if (d() == 'hw'){
        actual <- hw_forecast(df(), m())[[1]]
        pred <- hw_forecast(df(), m())[[2]]
        mse(actual, pred)
        create_df(actual, pred)
      }
      else{
        actual <- arima_forecast(df(), m())[[1]]
        pred <- arima_forecast(df(), m())[[2]]
        mse(actual, pred)
        create_df(actual, pred)
      }
    })
  })
})
# Run App -----------------------------------------------------------------
shinyApp(ui=ui, server=server)