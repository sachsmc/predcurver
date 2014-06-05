predcurver
========================================================

The aim of `predcurver` is to provide a predictiveness curve class. It can be used to estimate, print, summarize, and plot the predictiveness curve (Pepe, Feng, Huang, et al. 2008, Am J Epidemiol). Additionally, there are functions that compute summary statistics of the predictiveness curve: the total gain, and proportion of explained variation. Range-restricted versions of these measure are also possible, see (Sachs and Zhou 2013, Biometrical Journal) for details. Also implemented is the permutation test described in the same paper. 

To install, first install devtools from CRAN, then run

```
devtools::install_github("predcurver", username = "sachsmc")
```

To estimate the predictiveness curve, all you need is a vector of `risk` values (between 0 and 1). Then create the predictiveness curve objects with
```
predcurve(risk)
```

To run the Shiny application that comes with this package, you will need the `shiny` package from CRAN. Then the command is

```
library(shiny)
runGitHub("predcurver", "sachsmc", subdir = "inst/shinyapp/")
```

