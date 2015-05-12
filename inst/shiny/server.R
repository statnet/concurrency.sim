
require(RColorBrewer)
source('simplot.R')

shinyServer(function(input, output) {
    
  conc <- reactive({
    switch(input$conc,
           "No Concurrency" = "TT",
           "Female Only Concurrency" = "FT",
           "Male Only Concurrency" = "TF",
           "Both Sexes Concurrency" = "FF")
  })
  
  output$concplot <- renderPlot({
    sim.plot(md = input$md, 
             dur = input$dur, 
             conc = conc(), 
             medians = input$median,
             sims = input$sims, 
             simlines = input$simlines, 
             alpha = input$alpha, 
             leg = input$leg)
  })
  output$dlPlot <- downloadHandler(
    filename = 'ConcSimPlot.pdf',
    content = function(file) {
      pdf(file = file, h = 6, w = 10)
      par(mar=c(3.5, 3.5, 2.5, 1), mgp=c(2.2, 1, 0))
      sim.plot(md = input$md, 
               dur = input$dur,  
               conc = conc(),
               medians = input$median,
               sims = input$sims, 
               simlines = input$simlines, 
               alpha = input$alpha, 
               leg = input$leg)
      dev.off()
    }
  )
  
  
  # Stats tab
  output$statsm <- renderTable({
    sim.table(md = input$md, 
              dur = input$dur, 
              male=1, 
              digits=3)
  })
  output$statsf <- renderTable({
    sim.table(md=input$md, 
              dur=input$dur, 
              male=2, 
              digits=3)
  })
  
})