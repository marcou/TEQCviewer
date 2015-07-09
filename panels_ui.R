
load_data <- tabPanel("Load data",
                      inputPanel(fileInput("reads", "Reads")),
                      inputPanel(fileInput("coverage", "Coverage")),
                      inputPanel(fileInput("targets", "Targets"))
                      )

coverage_hist <- tabPanel("Histogram",
         inputPanel(textInput("region", "Region:", "1:123-456")),
         wellPanel(plotOutput("distPlot"))
)

coverage_plot <- tabPanel("Coverage",
                     inputPanel(textInput("region", "Region (chr:start-stop):", "1:123-456")),
                     wellPanel(plotOutput("covgPlot"))
)


# coverage <- tabPanel('coverage', 
#                      h6("Coverage on selected targets"), 
#                      selectInput("mut", 
#                                  label=" coverage on regions of interest: 
# select mutation", 
#                                  choices=levels(mut$name)
#                      ),
#                      plotOutput('cvg_mut')
# )
         
         