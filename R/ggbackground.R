##' set background for ggplot
##'
##'
##' @title ggbackground
##' @param gg gg object
##' @param background background image
##' @param ... additional parameter to manipulate background image, see also geom_image
##' @return gg object
##' @importFrom ggplot2 ggplot
##' @export
##' @author guangchuang yu
ggbackground <- function(gg, background, ...) {
    ## Add an isolated image layer as the first layer instead of wrapping `gg`
    ## in an annotation_custom grob.  Wrapping replaces the plot's coordinate
    ## system with the helper plot's 0:1 ranges, so annotations added after
    ## ggbackground() can be transformed to the wrong position (or clipped).
    ##
    ## The layer must not inherit the plot's global aesthetics: a mapping such
    ## as colour/alpha would otherwise be applied to the background once per
    ## observation and could tint or duplicate the image.
    background_layer <- geom_image(
        data = data.frame(x = 0.5, y = 0.5),
        mapping = ggplot2::aes(
            x = !!rlang::sym("x"),
            y = !!rlang::sym("y")
        ),
        inherit.aes = FALSE,
        image = background,
        size = Inf,
        ...
    )
    gg$layers <- c(list(background_layer), gg$layers)
    gg
}
