
library(concurrency.sim)

shinyServer(function(input, output) {

  monogM <- reactive({
    switch(input$conc,
           "No Concurrency" = TRUE,
           "Female Only Concurrency" = TRUE,
           "Male Only Concurrency" = FALSE,
           "Both Sexes Concurrency" = FALSE)
  })
  monogF <- reactive({
    switch(input$conc,
           "No Concurrency" = TRUE,
           "Female Only Concurrency" = FALSE,
           "Male Only Concurrency" = TRUE,
           "Both Sexes Concurrency" = FALSE)
  })

  sim <- reactive({
    conc_microsim(
         s.num.f = 1000,
         i.num.f = 50,
         s.num.m = 1000,
         i.num.m = 50,
         monog.f = monogF(),
         monog.m = monogM(),
         meandeg = input$md,
         part.duration = input$dur,
         nsteps = 2000,
         nsims = input$sims,
         verbose = FALSE)
  })

  output$concplot <- renderPlot({
    par(mar = c(3.5, 3.5, 1.2, 1), mgp = c(2.1, 1, 0))
    plot(sim(), alpha = input$alpha)
  })


})
