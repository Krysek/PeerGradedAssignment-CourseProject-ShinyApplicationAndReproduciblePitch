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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
     
    xIntercept <- input$xIntercept
    yIntercept <- input$yIntercept
    stretchCompress <- input$stretchCompress
    
    normalParabola <- calculateParabola(seq(-10,10,by=0.1),
                                  stretchCompress = 1,
                                  xIntercept = 0,
                                  yIntercept = 0)
    
    parabola <- calculateParabola(seq(-10,10,by=0.1),
                           stretchCompress = stretchCompress,
                           xIntercept = xIntercept,
                           yIntercept = yIntercept)
    
    vertex <- calculateParabola(0+xIntercept,
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
                 size=5, 
                 color="black", 
                 alpha=0.3) + 
       geom_line(data=parabola, 
                 aes(x=x, y=y), 
                 size=1,
                 color="blue") + 
       geom_vline(data=vertex, 
                  aes(xintercept = x),
                  colour = "red") +
       geom_hline(data=vertex, 
                  aes(yintercept=y),
                  colour = "red") +
       geom_point(data=vertex, 
                  aes(x=x, y=y), 
                  size = 5, 
                  shape = 4,
                  colour = "red")
    
  })
  
})

calculateParabola <- function(x, stretchCompress, xIntercept, yIntercept) {
   y <- stretchCompress * (xIntercept - x)^2 + yIntercept
   data.frame(x=x, y=y)
}