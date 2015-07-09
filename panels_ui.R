
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
                     inputPanel(textInput("region", "Region (chr:start-stop):", "1:11157025-11158264"),
                                actionButton("go1", label="refresh"),
                                checkboxInput('add_exons', 'Add exon data?', value = FALSE)),
                     wellPanel(plotOutput("covgPlot", 
                                          click = "plot_click",
                                          hover = hoverOpts(
                                            id = "plot_hover"
                                          ))),
                     wellPanel(textOutput("click_info"))
)

coverage_plot_own <- tabPanel("Coverage Two",
                          inputPanel(textInput("region2", "Region (chr:start-stop):", "1:20915200-20951500"),
                                     actionButton("go2", label="refresh"),
                                     checkboxInput('add_exons2', 'Add exon data?', value = FALSE)),
                          wellPanel(plotOutput("covgPlot_own"))
                          )

            