
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chainSTAT

<!-- badges: start -->

<!-- badges: end -->

chainSTAT is a small package that includes the functions needed to run
the seasonal adjustment code for national accounts by Statistics
Austria. The main objective are the chainlinking functions (using annual
overlap).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("statistikat/chainSTAT")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(chainSTAT)
#> Warning: package 'chainSTAT' was built under R version 4.0.3
# generating a cup and a pyp series
cup <- stats::ts(cumsum(c(100,stats::rnorm(99, mean = 1))), start = 1995, frequency = 4)
pyp <- cup / stats::rnorm(100, mean = 1.02, sd = 0.01)

# deriving volume series
vol <- chainlinkAO(cup,pyp)
```
