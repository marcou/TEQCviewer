
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TEQC)
library(ggplot2)

data_dir <- "example_data"
load(file.path(data_dir, "targets.RData"))
load(file.path(data_dir, "reads.RData"))
load(file.path(data_dir, "all_coverage.RData"))
source('helper-biomart.R')

shinyServer(function(input, output) {
  
  #source("coverage_server.R", local=TRUE)
  
user_data <- reactive({
  rds <- input$reads
  tgt <- input$targets
  cvg <- input$all_coverage
  
  if(is.null(c(input$reads, input$targets, input$all_coverage))){return(NULL)}
  
  load(rds$datapath)
  load(tgt$datapath)
  load(cvg$datapath)
})
  

  output$distPlot <- renderPlot({

  bp <- chrom.barplot(reads, targets) 
   bp

   
   
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = 30)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

output$covgPlot <- renderPlot({
  Start =  11157025  
  
  End = 11158264
  
  chr = 'chr1'
  
  bp <- plot_with_exons(all_coverage$coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human) 
  bp
  
  
  
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = 30)
  # 
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
  
})

})
