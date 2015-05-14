
library(concurrency.sim)

shinyServer(function(input, output, session) {

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

  observeEvent(input$conc, {
    mdConc <- ifelse(input$conc == "No Concurrency", 1, 1.5)
    updateSliderInput(session, inputId = "md", label = "Mean Degree",
                      min = 0.5, max = mdConc, value = 0.8, step = 0.05)
  })

  sim <- reactive({
    input$runMod

    progress <- shiny::Progress$new()
    progress$set(message = "Computing", value = 0)
    on.exit(progress$close())
    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1/input$sims, detail = detail)
    }

    isolate(
      conc_microsim(
           s.num.f = input$s.num.f,
           i.num.f = input$i.num.f,
           s.num.m = input$s.num.m,
           i.num.m = input$i.num.m,
           monog.f = monogF(),
           monog.m = monogM(),
           meandeg = input$md,
           part.duration = input$dur,
           nsteps = input$nsteps,
           nsims = input$sims,
           verbose = FALSE,
           updateProgress = updateProgress))
  })

  output$concplot <- renderPlot({
    par(mar = c(3.5, 3.5, 1.2, 1), mgp = c(2.1, 1, 0))
    plot(sim(),
         sim.lines = input$sim.lines,
         sim.alpha = input$sim.alpha,
         mean.line = input$mean.line,
         mean.smooth = input$mean.smooth,
         mean.lwd = input$mean.lwd,
         qnts = input$qnts,
         qnt.alpha = input$qnt.alpha)
  })


})
