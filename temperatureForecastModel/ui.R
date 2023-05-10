#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Global Mean Surface Temperature Prediction"),

    # Sidebar with a slider input for number years to predict
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Number of years to predict in the future:",
                        min = 1,
                        max = 10,
                        value = 5),
            
        ),

        # Show a plot of the generated forecast
        mainPanel(
                h3("Forecasted global mean temperature"),
                h4("Temperature ~ CO2 radiative forcing + Solar + ENSO + Aerosols"),
                plotOutput("forecastPlot"),
                br(),
                h3("Forecasted values of each independent variable"),
                h4("Radiative forcing from carbon dioxide"),
                h4("CO2 ~ quadradic trend + season"),
                plotOutput("CO2Plot"),
                h4("Solar output"),
                h4("Irradiance ~ trend + season + fourier(period=132, K=66)"),
                plotOutput("solarPlot"),
                h4("El Niño/Southern Oscillation"),
                h4("ENSO ~ trend + season + fourier(period=60, K=30"),
                plotOutput("ENSOPlot"),
                h4("Aerosols"),
                h4("AOD ~ trend + season"),
                plotOutput("aerosolsPlot")
        )        
    ),
    hr(),
    h5("Created by: Jim Milks"),
    br(),
    "10 May 2023",
    br(),
    a(actionButton(inputId = "email1", label = "Contact Admin", 
                   icon = icon("envelope", lib = "font-awesome")),
      href = "mailto: jrmilks@gmail.com"),
    br(),
    "Code available at:",
    a(href = "https://github.com/jrmilks74/global_temperature_forecast/tree/main", "https://github.com/jrmilks74/global_temperature_forecast/tree/main")
    
)
