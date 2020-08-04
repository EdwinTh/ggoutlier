
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggoutlier

<!-- badges: start -->

<!-- badges: end -->

A package with wrapper functions that deals with outliers, making the
regular range interpretable without abandoning the outliers.

## Installation

Currently this package is only available from this repo, install with:

``` r
# install.packages("devtools")
devtools::install_github("EdwinTh/ggoutlier")
```

## Example

The package contains a single function: `ggoutlier_hist`. More may be
added in the future.

``` r
library(ggoutlier)
library(ggplot2)
set.seed(1235)
x <- data.frame(some_var = c(rnorm(100), runif(10, -100, 100)))
ggplot(x, aes(some_var)) + geom_histogram()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# ggoutlier_hist(x, "some_var", -5, 5)
```
