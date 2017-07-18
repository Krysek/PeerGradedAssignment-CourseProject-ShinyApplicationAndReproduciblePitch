#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# function for caluclating parabola
calculateParabola <- function(x, stretchCompress, xIntercept, yIntercept) {
   y <- stretchCompress * (xIntercept - x)^2 + yIntercept
   data.frame(x=x, y=y)
}

# Define server logic required to draw a histogram
serverLogic <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
      xIntercept <- input$xIntercept
      yIntercept <- input$yIntercept
      stretchCompress <- input$stretchCompress
      
      normalParabola <- calculateParabola(seq(-10,10,by=0.1),
                                          stretchCompress = 1,
                                          xIntercept = 0,
                                          yIntercept = 0)
      
      normalParabolaVertex <- calculateParabola(0,
                                          stretchCompress = 1,
                                          xIntercept = 0,
                                          yIntercept = 0)
      
      parabola <- calculateParabola(seq(-10,10,by=0.1),
                                    stretchCompress = stretchCompress,
                                    xIntercept = xIntercept,
                                    yIntercept = yIntercept)
      
      parabolaVertex <- calculateParabola(0+xIntercept,
                                  stretchCompress = stretchCompress,
                                  xIntercept = xIntercept,
                                  yIntercept = yIntercept)
      
      # draw the histogram with the specified number of bins
      ggplot() +
         ggtitle(paste0("y = ",
                        stretchCompress, "*(", 
                        xIntercept, 
                        "-x)^2+(", 
                        yIntercept,
                        ")")) + 
         coord_cartesian(xlim = c(-10, 10), 
                         ylim = c(-10, 10)) +
         geom_line(data=normalParabola, aes(x=x, y=y), 
                   size=2, 
                   color="black", 
                   alpha=0.3) + 
         geom_vline(data=normalParabolaVertex, 
                    aes(xintercept = x), 
                    size=2
                    , 
                    color="black", 
                    alpha=0.3) +
         geom_hline(data=normalParabolaVertex, 
                    aes(yintercept=y), 
                    size=2, 
                    color="black", 
                    alpha=0.3) +
         geom_point(data=normalParabolaVertex, 
                    aes(x=x, y=y), 
                    size=6, 
                    shape = 4,
                    color="black", 
                    alpha=0.3) +
         geom_line(data=parabola, 
                   aes(x=x, y=y), 
                   size=1,
                   color="blue") + 
         geom_vline(data=parabolaVertex, 
                    aes(xintercept = x),
                    colour = "red") +
         geom_hline(data=parabolaVertex, 
                    aes(yintercept=y),
                    colour = "red") +
         geom_point(data=parabolaVertex, 
                    aes(x=x, y=y), 
                    size = 5, 
                    shape = 4,
                    colour = "red")
      
   }, height = 300, width = 300)
   
}

# Add serverLogic
shinyServer(serverLogic)
