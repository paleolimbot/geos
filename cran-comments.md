
Release to prepare for an updated version of libgeos, which adds features
from GEOS 3.9.1.

## Test environments

* local R installation, R 4.0.3
* GitHub Actions: windows-latest (oldrel), windows-latest (release),
  macOS-latest (release), ubuntu-20.04 (release), ubuntu-20.04 (devel)
* win-builder (devel)
* `rhub::check_for_cran()`
* `rhub::check(platform = 'ubuntu-rchk')`

## R CMD check results

0 errors | 0 warnings | 0 notes
