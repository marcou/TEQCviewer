
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TEQC)
library(ggplot2)

shinyServer(function(input, output) {
  
  #source("coverage_server.R", local=TRUE)
  

  output$distPlot <- renderPlot({
    
    if(is.null(c(input$reads, input$targets, input$all_coverage)))
       return(NULL)
    
   rds <- input$reads
   tgt <- input$targets
   rds <- load(rds$datapath)
   tgt <- load(tgt$datapath)
   
   bp <- chrom.barplot(rds(), tgt())
   bp

   
   
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = 30)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
