
load_data <- tabPanel("Load data",
                      inputPanel(fileInput("reads", "Reads")),
                      inputPanel(fileInput("coverage", "Coverage")),
                      inputPanel(fileInput("targets", "Targets")),
                      includeMarkdown("upload_info.md")
                      )

coverage_hist <- tabPanel("Histogram",
         #inputPanel(textInput("region", "Region:", "1:123-456")),
         wellPanel(plotOutput("distPlot"))
)

coverage_plot <- tabPanel("Coverage",
                     inputPanel(textInput("region", "Region (chr:start-stop):", "1:11157025-11158264")),
                     wellPanel(plotOutput("covgPlot", 
                                          click = "plot_click",
                                          hover = hoverOpts(
                                            id = "plot_hover"
                                          ))),
                     wellPanel(textOutput("click_info"))
)

coverage_plot_own <- tabPanel("Coverage Two",
                          inputPanel(textInput("region", "Region (chr:start-stop):", "1:11157025-11158264")),
                          wellPanel(plotOutput("covgPlot_own"))
                          )

            