output$cvg_mut <- renderPlot({   
  
  lvls <- which(levels(mut$name)==input$mut)
  matched <- match(levels(mut$name), mut$name)
  idx <- matched[lvls]
  
  chr <- sub("chr", "", mut[idx, 1])
  Start <- mut[idx, 2]
  End <- mut[idx, 3]
  cvgA <- cvg$coverageAll
  qp <- qplot(Start:End, cvgA[[chr]][Start:End], geom = "line",
              main=input$mut) + #ylim...
    geom_hline(yintercept=0, lty=3) +
    geom_hline(yintercept=100, lty=3, col=4) +
    scale_x_continuous(paste("position on Chromosome", chr)) +
    scale_y_continuous("Coverage")
  qp
})