# concurrency.sim

**concurrency.sim** is an R package that contains a microsimulation model for the effects of sexual partner concurrency on HIV-1 transmission dynamics. It provides a limited set of analysis and plotting tools, as well as a built-in Shiny application, to explore concurrency within a heterosexual population at risk.

<img src="https://github.com/statnet/concurrency.sim/raw/master/inst/ConcEx.png">

You can run the Shiny app locally, or online from the shinyapps.io server in your web browser.

### Local Installation
The package is hosted on Github. It can be installed using the <a href="https://github.com/hadley/devtools" target="_blank">devtools package</a>:
```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("statnet/concurrency.sim")
```

Then, to start the built-in Shiny application on your local machine, load the package and run the `concweb` function.
```r
library(concurrency.sim)
concweb()
```
<img src="https://github.com/statnet/concurrency.sim/raw/master/inst/ShinySS.png">

### Online Shiny App
This app is also hosted online at [shinyapps.io](https://statnet.shinyapps.io/ConcurrencySim/).  Connecting via the shinyapps.io server allows you to run the app in your web browser without having to install `R` or this `concurrency.sim` package on your computer.
