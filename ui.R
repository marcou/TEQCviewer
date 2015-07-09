
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TEQC)
library(ggplot2)

source("panels_ui.R")


shinyUI(fluidPage(
  headerPanel("TEQCviewer"),
  mainPanel(
    tabsetPanel(load_data, coverage_hist, coverage_plot, coverage_plot_own
  ))))





