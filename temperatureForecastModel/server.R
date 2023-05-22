#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Packages
library(shiny)
library(tidyverse)
library(fpp3)

# Load data
source("forecast_data.R")

# Define server logic
shinyServer(function(input, output) {
        
        # Define reactivity variable
        h_selected <- reactive({
                if (input$years == "1")
                        12
                else if (input$years == "2")
                        24
                else if (input$years == "3")
                        36
                else if (input$years == "4")
                        48
                else if (input$years == "5")
                        60
                else if (input$years == "6")
                        72
                else if (input$years == "7")
                        84
                else if (input$years == "8")
                        96
                else if (input$years == "9")
                        108
                else 
                        120
        })
        
        # Forecast plot
        
        output$forecastPlot <- renderPlot({
                
                # Create predicted data for temperature prediction
                predictions_for_future <- scenarios(
                        CO2_fc <- forecast(CO2_fit, h = h_selected()),
                        irradiance_fc <- forecast(irradiance_fit, h = h_selected()),
                        aerosols_fc <- forecast(aerosols_fit, h = h_selected()),
                        ENSO_fc <- forecast(ENSO_fit, h = h_selected()),
                        Future = new_data(global_temp, h_selected()) %>%
                                mutate(CO2 = CO2_fc$.mean,
                                       Solar = irradiance_fc$.mean,
                                       ENSO = ENSO_fc$.mean,
                                       Aerosols = aerosols_fc$.mean) %>%
                                update_tsibble(index = time)
                )
                
                # Forecast model for global temperatues
                global_temp_forecast <- forecast(fit_temp, new_data = predictions_for_future$Future)
                
                # Plot the forecast
                p <- global_temp %>%
                        autoplot(Temperature) +
                        autolayer(global_temp_forecast) +
                        theme_bw() +
                        labs(title = "Predicted global mean temperature",
                             x = "Year",
                             y = "Temperature anomaly (ºC)")
                p
        })
        
        output$CO2Plot <- renderPlot({
                CO2_fc <- forecast(CO2_fit, h = h_selected())
                pCO2 <- global_temp %>%
                        autoplot(CO2) +
                        autolayer(CO2_fc) +
                        theme_bw() +
                        labs(title = "Predicted atmospheric carbon dioxide radiative forcing",
                             x = "Year",
                             y = "Watts per square meter")
                pCO2
        })
        
        output$solarPlot <- renderPlot({
                irradiance_fc <- forecast(irradiance_fit, h = h_selected())
                pSolar <- global_temp %>%
                        autoplot(Solar) +
                        autolayer(irradiance_fc) +
                        theme_bw() +
                        labs(title = "Predicted solar activity",
                             x = "Year",
                             y = "Solar irradiance (Watts per square meter")
                pSolar
        })
        
        output$ENSOPlot <- renderPlot({
                ENSO_fc <- forecast(ENSO_fit, h = h_selected())
                global_temp %>%
                        autoplot(ENSO) +
                        autolayer(ENSO_fc)
                        theme_bw() +
                        labs(title = "Predicted ENSO",
                             x = "Year",
                             y = "Water temperature anomaly (ºC)")
        })
        
        output$aerosolsPlot <- renderPlot({
                aerosols_fc <- forecast(aerosols_fit, h = h_selected())
                pAerosols <- global_temp %>%
                        autoplot(Aerosols) +
                        autolayer(aerosols_fc) +
                        theme_bw() +
                        labs(title = "Predicted atmospheric aerosol levels",
                             x = "Year",
                             y = "Atmospheric optical depth")
                pAerosols
        })
})


