<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggimage: Use Images in ‘ggplot2’

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggimage?color=green)](https://cran.r-project.org/package=ggimage)
![](http://cranlogs.r-pkg.org/badges/grand-total/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/ggimage?color=green)
![](http://cranlogs.r-pkg.org/badges/last-week/ggimage?color=green)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.344625.svg)](https://doi.org/10.5281/zenodo.344625)

`ggimage` brings images and graphics into `ggplot2`—from a picture at
each observation to a full-panel background or a miniature plot embedded
in the data.

With `ggimage`, you can:

- **Map images to data:** place local files, URLs, or generated image
  sources at data coordinates, with control over size, aspect ratio,
  rotation, colour, and transparency.
- **Build image-based plots:** add backgrounds with `geom_bgimage()` and
  embed plots or other graphic objects as subviews with
  `geom_subview()`.
- **Use ready-made image sources:** draw flags, icons, emoji, Pokemon,
  or PhyloPic images without manually assembling image layers.
- **Reduce overlap:** use `geom_image_repel()` to separate crowded image
  marks.
- **Make image plots interactive:** use `geom_image_interactive()` or
  `geom_phylopic_interactive()` to add hover and selection interactions
  in HTML output via `ggiraph`.

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

For examples and details, see the [package
vignette](https://cran.r-project.org/web/packages/ggimage/vignettes/ggimage.html).
