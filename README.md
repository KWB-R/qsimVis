[![R-CMD-check](https://github.com/KWB-R/qsimVis/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/qsimVis/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/qsimVis/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/qsimVis/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/qsimVis/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/qsimVis)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/qsimVis)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/qsimVis)](https://kwb-r.r-universe.dev/)

# qsimVis

Aggregate and visualize Qsim/Hydrax output data

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'qsimVis' from GitHub
remotes::install_github("KWB-R/qsimVis")
```

## Documentation

Release: [https://kwb-r.github.io/qsimvis](https://kwb-r.github.io/qsimvis)

Development: [https://kwb-r.github.io/qsimvis/dev](https://kwb-r.github.io/qsimvis/dev)
