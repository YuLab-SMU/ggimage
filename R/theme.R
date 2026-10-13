##' transparent background theme
##'
##'
##' @title theme_transparent
##' @param ... additional parameter to tweak the theme
##' @return ggplot object
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_rect
##' @export
##' @author Guangchuang Yu with contributions from Hugo Gruson
theme_transparent <- function(...) {
    theme(panel.background = element_rect(
              fill = "transparent",
              colour = NA),
          plot.background = element_rect(
              fill = "transparent",
              colour = NA),
          legend.key = element_rect(
              fill = "transparent",
              colour = NA),
          legend.background = element_rect(
              fill = "transparent",
              colour = NA),
          ## facet strips inherit the opaque panel background from the base
          ## theme, so a facetted plot left one grey band across the top
          strip.background = element_rect(
              fill = "transparent",
              colour = NA), ...)
}



##' A theme that only show the plot panel
##'
##'
##' @title theme_nothing
##' @param base_size font size
##' @param base_family font family
##' @importFrom ggplot2 %+replace%
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme_void
##' @return ggplot2 theme
##' @export
##' @author Guangchuang Yu
theme_nothing <- function(base_size = 11, base_family = "") {
    theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(plot.margin=unit(c(0,0, -.2, -.2), "lines"))
}
