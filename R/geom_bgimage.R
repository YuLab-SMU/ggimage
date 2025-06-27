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
ggplot_add.bgimage <- function(object, plot, ...) {
    ly <- geom_image(image = object$image, size = Inf)
    plot$layers <- c(ly, plot$layers)
    return(plot )
}
