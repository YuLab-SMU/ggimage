<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggimage: Use Images in ‘ggplot2’

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggimage?color=green)](https://cran.r-project.org/package=ggimage)
![](http://cranlogs.r-pkg.org/badges/grand-total/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/last-week/ggimage?color=green)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.344625.svg)](https://doi.org/10.5281/zenodo.344625)

Supports image files and graphic objects to be visualized in ‘ggplot2’
graphic system. Images can be drawn at data points, used as the
background of a plot, or embedded as a sub-plot, and can be read from
local files or downloaded from remote services such as ‘Pokemon’,
‘PhyloPic’, and emoji and icon collections. An interactive geom is
provided for ‘HTML’ output.

## :writing_hand: Authors

Guangchuang YU <https://yulab-smu.top>

School of Basic Medical Sciences, Southern Medical University

## :arrow_double_down: Installation

Get the released version from CRAN:

``` r
## include BioC repo for dependencies
setRepositories(ind=1:2)
install.packages("ggimage")
```

Or the development version from github:

``` r
setRepositories(ind=1:2)
## install.packages("devtools")
devtools::install_github("YuLab-SMU/ggimage")
```

## :book: Vignette

For more details, please refer to the [online
vignette](https://yulab-smu.top/pkgdocs/ggimage.html).
