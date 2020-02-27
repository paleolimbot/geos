
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geom

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![R-CMD-check](https://github.com/paleolimbot/geom/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The goal of geom is to provide low-level access to the GEOS library.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/geom")
```

If you can load the package, you’re good to go\!

``` r
library(geom)
```

## Example

Right now the package doesn’t do much except between well-known text and
well-known binary.

``` r
(wkb <- as_geo_wkb(geo_wkt("POINT (1 0)")))
#> <geo_wkb [1]>
#> <raw [21]>
as_geo_wkt(wkb)
#> <geo_wkt[1]>
#> [1] POINT (1.0000000000000000 0.0000000000000000)
```
