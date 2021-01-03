
Maintenance release to support forthcoming updates to suggeested
packages.

## Test environments

* local R installation, R 4.0.3
* GitHub Actions: windows-latest (oldrel), windows-latest (release),
  macOS-latest (release), ubuntu-18.04 (release), ubuntu-18.04 (devel)
* win-builder (devel)
* `rhub::check_for_cran()`
* `rhub::check(platform = 'ubuntu-rchk')`
* `rhub::check_with_sanitizers()`

## R CMD check results

0 errors | 0 warnings | 0 notes
