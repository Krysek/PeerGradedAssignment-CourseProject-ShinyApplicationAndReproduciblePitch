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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Parabola"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("stretchCompress",
                   "Stretch(>1.0) or compress(<1.0):",
                   min = 0.2,
                   max = +1.8,
                   step = 0.2,
                   value = 1),
       sliderInput("xIntercept",
                   "X Intercept:",
                   min = -10,
                   max = 10,
                   step = 0.5,
                   value = 0),
       sliderInput("yIntercept",
                   "Y Intercept:",
                   min = -10,
                   max = +10,
                   step = 0.5,
                   value = 0)
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
