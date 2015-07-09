
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TEQC)
library(ggplot2)
library(stringr)

data_dir <- "example_data"
load(file.path(data_dir, "targets.RData"))
load(file.path(data_dir, "reads.RData"))
load(file.path(data_dir, "all_coverage.RData"))
x_reads <- reads
x_targets <- targets
x_all_coverage <- all_coverage
source('helper-biomart.R')

shinyServer(function(input, output) {
  
  #source("coverage_server.R", local=TRUE)
  
  user_targets <- reactive({
    if(is.null(input$targets)){return(x_targets)} else {
      load(input$targets$datapath)
      return(targets)}
  })
  
  user_reads <- reactive({
    if(is.null(input$reads)){return(x_reads)} else {
      load(input$reads$datapath)
      return(reads)}
  })
  
  user_coverage <- reactive({
    if(is.null(input$coverage)){return(x_all_coverage)} else {
      load(input$coverage$datapath)
      return(all_coverage)}
  })
  
  

  output$distPlot <- renderPlot({

  bp <- chrom.barplot(user_reads(), user_targets()) 
   bp

  })

  
  data_to_plot = reactive({
    region = str_split(input$region,'\\:')[[1]]
    chr = paste('chr',region[1],sep='')
    
    Start = as.integer(str_split(region[2], '\\-')[[1]][1])
    End = as.integer(str_split(region[2], '\\-')[[1]][2])
    
    prep_plot_with_exons(all_coverage$coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human) 
    
  })

output$covgPlot <- renderPlot({
  
 # all_coverage <- user_coverage()
 #  targets <- user_targets()
  
  add_exons = input$add_exons
  region = str_split(input$region,'\\:')[[1]]
  chr = paste('chr',region[1],sep='')
  
  Start = as.integer(str_split(region[2], '\\-')[[1]][1])
  End = as.integer(str_split(region[2], '\\-')[[1]][2])
#   
#   data_to_plot =  prep_plot_with_exons(all_coverage$coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human) 
 
  
#   Start =  11157025  
#   
#   End = 11158264
#   
#   chr = 'chr1'
  
  bp <- draw_plot_with_exons(data_to_plot(),targets,chr,Start,End, ensembl_in=ensembl_human, add_exons=add_exons) 
  bp

})

output$covgPlot_own <- renderPlot({
  
  all_coverage <- user_coverage()
   targets <- user_targets()
#   
#   
add_exons = input$add_exons2
  region = str_split(input$region2,'\\:')[[1]]
  chr = paste('chr',region[1],sep='')
  
  Start = as.integer(str_split(region[2], '\\-')[[1]][1])
  End = as.integer(str_split(region[2], '\\-')[[1]][2])
  
   data_to_plot = prep_plot_with_exons(all_coverage$coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human) 
  
  bp <- draw_plot_with_exons(data_to_plot,targets,chr,Start,End, ensembl_in=ensembl_human, add_exons=add_exons) 
  bp
  
})



output$click_info <- renderText({
  region = str_split(input$region,'\\:')[[1]]
  chr = paste('chr',region[1],sep='')
#   
#   Start = as.integer(str_split(region[2], '\\-')[[1]][1])
#   End = as.integer(str_split(region[2], '\\-')[[1]][2])
#   
#   data_to_plot = prep_plot_with_exons(all_coverage$coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human) 
  
  nearest = nearPoints(data_to_plot(), input$plot_click, addDist = TRUE)
  
   paste("Selected location:\n",chr,':',nearest$x[1],' coverage=',nearest$y[1],sep='')
})
output$hover_info <- renderPrint({
  cat("input$plot_hover:\n")
  str(input$plot_hover)
})

})
