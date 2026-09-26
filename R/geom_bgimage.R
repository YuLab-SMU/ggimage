##' add image as background to plot panel.
##'
##' 
##' @title geom_bgimage
##' @param image image file
##' @return ggplot
##' @export
##' @author Guangchuang Yu
geom_bgimage <- function(image) {
    structure(list(image = image), class = 'bgimage')
}

##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add bgimage
##' @export
ggplot_add.bgimage <- function(object, plot, object_name, ...) {
    background_layer <- geom_image(
        data = data.frame(x = 0.5, y = 0.5),
        mapping = ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,
        image = object$image,
        size = Inf
    )
    plot$layers <- c(list(background_layer), plot$layers)
    return(plot)
}
