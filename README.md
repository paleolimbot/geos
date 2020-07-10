
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geos

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![R-CMD-check](https://github.com/paleolimbot/geos/workflows/R-CMD-check/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/geos/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/geom?branch=master)
<!-- badges: end -->

The goal of geom is to provide [access to the GEOS C
API](https://geos.osgeo.org/doxygen/geos__c_8h_source.html) by
vectorizing the C functions for use in R.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/geos")
```

If you can load the package, you’re good to go\!

``` r
library(geos)
```

## Example

Create and export a line\!

``` r
(line <- geos_read_wkt("LINESTRING (30 10, 10 30, 40 40)"))
#> <geos_geometry[1]>
#> [1] LINESTRING (30 10, 10 30, 40 40)
geos_write_wkt(line)
#> [1] "LINESTRING (30 10, 10 30, 40 40)"
```

Operators are a work in progress. ([browse the last commit before the
rewrite](https://github.com/paleolimbot/geos/tree/bf3ef50a0f01851e2b55b0060d38754495697815)).
