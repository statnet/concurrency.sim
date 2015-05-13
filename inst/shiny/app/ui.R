
library(concurrency.sim)

shinyUI(pageWithSidebar(

  # Header
  headerPanel("Microsimulation Model of Concurrency on HIV Transmission Dynamics"),

  # Sidebar
  sidebarPanel(

    h4("Epidemic Parameters"),

    sliderInput(inputId = "md",
                label = "Mean Degree",
                min = 0.5,
                max = 1.5,
                value = 0.8,
                step = 0.05),
    br(),
    sliderInput(inputId = "dur",
                label = "Mean Partnership Duration (months)",
                min = 1,
                max = 50,
                value = 10,
                step = 1),
    br(),
    selectInput(inputId = "conc",
                label = "Concurrency Rule",
                choices = c("No Concurrency",
                            "Female Only Concurrency",
                            "Male Only Concurrency",
                            "Both Sexes Concurrency")),

    br(), br(),

    h4("Graphical Parameters"),
    sliderInput(inputId = "sims",
                label = "Number of Simulations",
                value = 10,
                min = 1,
                max = 25,
                step = 1),
    br(),
    checkboxInput(inputId = "simlines",
                  label = "Plot Individual Simulations",
                  value = TRUE),
    sliderInput(inputId = "alpha",
                label = "Simulation Line Transparency",
                value = 0.3,
                min = 0.1,
                max = 1,
                step = 0.05),
    checkboxInput(inputId = "leg",
                  label = "Plot Legend",
                  value = TRUE)
  ),

  # Main panel
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               h4("Plot of Simulation Results"),
               plotOutput(outputId = "concplot")
      ),
      tabPanel("About",
               p("This application simulates a stochastic epidemic model of HIV-1 infection to
                 illustrate the impact of sexual partnership concurrency on transmission dynamics.
                 The theory and mathematics of this model are provided in the full ",
                 a("Concurrency Tutorial",
                   href="https://statnet.csde.washington.edu/trac/wiki/ConcurrencyIndex"),
                 " associated with this web application. This model is also built into the",
                 a("EpiModel", href="http://cran.r-project.org/web/packages/EpiModel/index.html"),
                 " software available for the R statistical computing platform."),
               p("HIV infection is simulated based on a four-stage disease progression model
                   in which persons transition from acute to latent to pre-AIDS to AIDS stages.
                   These transitions occur at deterministic intervals based on estimates of the
                   average time per stage. Also, the transmission probability to uninfected partners
                   varies by stage of the infected partner: it is highest in the acute stage and
                   lowest in the AIDS stage when no sexual acts are simulated to occur. This is
                   based on empirical estimates in ",
                 a("Hollingsworth (2008).", href="http://www.ncbi.nlm.nih.gov/pubmed/18662132")),
               strong("Authors"),
               p("These tools were authored by ",
                 a("Steven M. Goodreau,", href="http://faculty.washington.edu/goodreau/"),
                 " ",
                 a("Samuel M. Jenness,", href="http://samueljenness.org/"),
                 " and ",
                 a("Martina Morris", href="http://faculty.washington.edu/morrism/"))
      )

    )
  )
))
