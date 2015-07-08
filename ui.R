
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TEQC)
library(ggplot2)

#source(coverage_ui.R)


shinyUI(fluidPage(
  headerPanel("TEQCviewer"),
  mainPanel(
    tabsetPanel(
      tabPanel("header",
               inputPanel(textInput("region", "Region:", "1:123-456")),
               wellPanel(plotOutput("distPlot"))
    )
  ))))





