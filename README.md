
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinySbm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`shinySbm` is a package containing a shiny application. This application
is build for network analysis based on the `sbm` package
[CRAN](https://CRAN.R-project.org/package=sbm). It allow to apply and
explore the outputs of a Stochastic Block Model. It is useful if you
want to analyse your data without `R` language knowledge or to learn the
basic lines of the `sbm` package.

## Installation

You can install the development version of shinySbm like so:

``` r
remotes::install_github("Jo-Theo/shinySbm")
```

The shinySbm package should be installed.

## Running The Application

From a new `R` session you can then run

``` r
shinySbm::run_app()
```
