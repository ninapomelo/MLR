MLR Package
=======

<!-- badges: start -->
[![R-CMD-check](https://github.com/ninapomelo/MLR/workflows/R-CMD-check/badge.svg)](https://github.com/ninapomelo/MLR/actions)

[![test-coverage](https://github.com/ninapomelo/MLR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ninapomelo/MLR/actions/workflows/test-coverage.yaml)
<!-- badges: end -->


## Overview
This package is created for the purpose of BIOSTAT 625 HW4 and is intended to provides comparable analytical results for multiple linear regression, mimicking the more commonly known `lm()` function, and `anova()` function. Included in the package is the function `mlr()`, which can provide a list containing model details, fitted values of the model, the residuals of the model, the estimated coefficients, F statistic,variance-covariance matrix, ANOVA table and R_Square.

## Installation
```{r}
devtools::install_github("ninapomelo/MLR")
library(MLR)
```

## Useage
The main function in the package `MLR` is `mlr`.

The output is a list of analytical results, which containing the following seven objects:

* `y.fitted:`: the fitted values of the model, as a vector
* `y.res`: the residuals of the model, as a vector
* `summary`: the estimated coefficients, as a table, which also contains the results of a two-sided t-test
* `FS`: F statistic 
* `vcov.matrix`: the variance-covariance matrix
* `ANOVA.table`: the results of an ANOVA test, as a list
* `R_Square`: the R-squared and adjusted R-square, as a vector of length 2


#### Quick Example
```{r}
mlr("mpg", c("wt","hp","vs"), data=mtcars)
```
## Authorship
This package was created in 2020 by Yue Pan.

<panyue@umich.edu>



