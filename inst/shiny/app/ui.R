
library(concurrency.sim)

shinyUI(fluidPage(

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
      uiOutput("mdControl"),
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
