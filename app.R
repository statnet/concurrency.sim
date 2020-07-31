
library(shiny)

ui <- (fluidPage(
  
  # Header
  titlePanel("Microsimulation Model of Concurrency on HIV Transmission Dynamics"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      h3("Instructions", style = "margin-top: 0px"),
      helpText("Click Run Model after changing model parameters",
               "or conditions."),
      actionButton(inputId = "runMod", "Run Model"),
      
      h4("Initial Conditions", style = "margin-top: 25px"),
      numericInput(inputId = "s.num.m", label = "n Male Susceptible",
                   value = 1000, min = 1),
      numericInput(inputId = "s.num.f", label = "n Female Susceptible",
                   value = 1000, min = 1),
      numericInput(inputId = "i.num.m", label = "n Male Infected",
                   value = 50, min = 1),
      numericInput(inputId = "i.num.f", label = "n Female Infected",
                   value = 50, min = 1),
      
      h4("Epidemic Parameters", style = "margin-top: 25px"),
      sliderInput(inputId = "md", label = "Mean Degree",
                  min = 0.5, max = 1, value = 0.8, step = 0.05),
      sliderInput(inputId = "dur", label = "Mean Partnership Duration",
                  min = 1, max = 50, value = 10, step = 1),
      selectInput(inputId = "conc", label = "Concurrency Rule",
                  choices = c("No Concurrency",
                              "Female Only Concurrency",
                              "Male Only Concurrency",
                              "Both Sexes Concurrency")),
      
      h4("Control Settings", style = "margin-top: 25px"),
      sliderInput(inputId = "sims", label = "Simulations",
                  value = 10, min = 1, max = 25, step = 1),
      numericInput(inputId = "nsteps", label = "Time Steps",
                   value = 2000, min = 100)
    ), # end sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 h4("Simulation Results"),
                 plotOutput(outputId = "concplot", height = "550px"),
                 br(),
                 wellPanel(
                   h4("Graphical Parameters"),
                   fluidRow(
                     column(3, checkboxInput(inputId = "sim.lines", label = "Sim Lines",
                                             value = FALSE)),
                     column(3, checkboxInput(inputId = "mean.line", label = "Mean Line",
                                             value = TRUE)),
                     column(3, checkboxInput(inputId = "mean.smooth", label = "Smooth Mean",
                                             value = TRUE)),
                     column(3, checkboxInput(inputId = "qnts.smooth", label = "Smooth Quantile",
                                             value = TRUE))
                   ),
                   fluidRow(
                     column(6, sliderInput(inputId = "qnts", label = "Quantile Range",
                                           value = 0.5, min = 0, max = 1, step = 0.05)),
                     column(6, sliderInput(inputId = "sim.alpha", label = "Sim Line Transparency",
                                           value = 0.3, min = 0.05, max = 1, step = 0.05))
                   ),
                   fluidRow(
                     column(6, sliderInput(inputId = "mean.lwd", label = "Mean Width",
                                           value = 3, min = 1, max = 5, step = 1)),
                     column(6, sliderInput(inputId = "qnt.alpha", label = "Quantile Transparency",
                                           value = 0.3, min = 0.05, max = 1, step = 0.05))
                   )
                 )),
        tabPanel("About",
                 p("This application simulates a stochastic epidemic model of HIV-1 infection to
         illustrate the impact of sexual partnership concurrency on transmission dynamics.
         The theory and mathematics of this model are provided in the full ",
                   a("Concurrency Tutorial",
                     href = "https://statnet.csde.washington.edu/trac/wiki/ConcurrencyIndex"),
                   " associated with this web application. This model is also built into the",
                   a("EpiModel", href = "http://cran.r-project.org/web/packages/EpiModel/index.html"),
                   " software available for the R statistical computing platform.", style = "margin-top: 25px"),
                 p("HIV infection is simulated based on a four-stage disease progression model
           in which persons transition from acute to latent to pre-AIDS to AIDS stages.
           These transitions occur at deterministic intervals based on estimates of the
           average time per stage. Also, the transmission probability to uninfected partners
           varies by stage of the infected partner: it is highest in the acute stage and
           lowest in the AIDS stage when no sexual acts are simulated to occur. This is
           based on empirical estimates in ",
                   a("Hollingsworth (2008).", href = "http://www.ncbi.nlm.nih.gov/pubmed/18662132")),
                 strong("Authors"),
                 p("These tools were authored by ",
                   a("Steven M. Goodreau,", href = "http://faculty.washington.edu/goodreau/"),
                   " ",
                   a("Samuel M. Jenness,", href = "http://samueljenness.org/"),
                   " and ",
                   a("Martina Morris", href = "http://faculty.washington.edu/morrism/")))
      ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
  
))



server <- (function(input, output, session) {
  
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
         qnt.alpha = input$qnt.alpha,
         qnts.smooth = input$qnts.smooth)
  })
  
  
})

shinyApp(ui = ui, server = server)