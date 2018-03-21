# concurrency.sim

**concurrency.sim** is an R package that contains a microsimulation model for the effects of sexual partner concurrency on HIV-1 transmission dynamics. It provides a limited set of analysis and plotting tools, as well as a built-in Shiny application, to explore concurrency within a heterosexual population at risk.

### Installation
This software is currently hosted on Github only. It can be installed using the <a href="https://github.com/hadley/devtools" target="_blank">devtools package</a>:
```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("statnet/concurrency.sim")
```

### Shiny App
To run the built-in Shiny application, load the package and then run the `concweb` function.
```r
library(concurrency.sim)
concweb()
```
<img src="https://github.com/statnet/concurrency.sim/raw/master/inst/ShinySS.png">
