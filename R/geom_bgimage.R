##' add image as background to plot panel.
##'
##'
##' @title geom_bgimage
##' @param image image file, or a magick image object
##' @param pixel the pixel of `image` to anchor, given as `c(col, row)` counted
##' from the top-left corner of the image, i.e. `col` pixels from the left and
##' `row` pixels from the top. The centre of that pixel is placed on the data
##' point `(x, y)`. If `NULL` (the default) the image is stretched to fill the
##' plot panel and the remaining parameters are ignored.
##' @param x x coordinate of the data point that `pixel` is anchored to. Only
##' used when `pixel` is given.
##' @param y y coordinate of the data point that `pixel` is anchored to. Only
##' used when `pixel` is given.
##' @param scale number of data units per image pixel. Required when `pixel` is
##' given; it controls the size of the image in data coordinates. To make the
##' image `w` data units wide, use `scale = w / image_info(image)$width`. The
##' extent is given in data units on both axes, so the image keeps its
##' proportions only under `coord_fixed()`, and is otherwise stretched by the
##' panel's physical shape.
##' @return An object that `+` adds to a plot as a background layer. The
##' background never takes part in the scale ranges: with `pixel = NULL` it
##' covers the panel whatever the data, and with `pixel` given the axes can be
##' discrete as well as continuous.
##' @export
##' @author Guangchuang Yu
##' @examples
##' library("ggplot2")
##' library("ggimage")
##'
##' img <- system.file("extdata/Rlogo.png", package = "ggimage")
##' d <- data.frame(x = c(1, 2, 3), y = c(1, 2, 1.5))
##' p <- ggplot(d, aes(x, y)) +
##'     scale_x_continuous(limits = c(0, 4)) +
##'     scale_y_continuous(limits = c(0, 4))
##'
##' ## by default the image is stretched to fill the panel
##' p + geom_bgimage(img) + geom_point()
##'
##' ## with `pixel`, the image is placed in data coordinates instead: the
##' ## centre of the bottom-left pixel is anchored to the origin, and `scale`
##' ## sets the size
##' info <- magick::image_info(magick::image_read(img))
##' p + geom_bgimage(img, pixel = c(1, info$height), x = 0, y = 0,
##'                  scale = 4 / info$width) + geom_point()
geom_bgimage <- function(image, pixel = NULL, x = 0, y = 0, scale = NULL) {
    structure(list(image = image, pixel = pixel, x = x, y = y, scale = scale),
              class = 'bgimage')
}

##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add bgimage
##' @export
ggplot_add.bgimage <- function(object, plot, object_name, ...) {
    background_layer <- if (is.null(object$pixel)) {
        ## `-Inf`/`Inf` cover the panel without carrying an x or y value, so
        ## the background neither trains a scale nor fails on a discrete axis.
        ggplot2::annotation_raster(bgimage_raster(object$image),
                                   -Inf, Inf, -Inf, Inf)
    } else {
        bgimage_anchor_layer(object$image, object$pixel, object$x, object$y,
                             object$scale)
    }
    plot$layers <- c(list(background_layer), plot$layers)
    return(plot)
}

## A path is read here rather than by `annotation_raster()`, which only takes a
## raster object; vector files get the same density as `geom_image()`.
bgimage_raster <- function(image) {
    if (is.character(image)) {
        image <- .read_image_file(image)
    }
    image
}

## Place `image` in data coordinates so that the centre of its `pixel` lands on
## `(x, y)`.
##
## A pixel is a cell, not a grid line: pixel column `ax` covers the interval
## [ax - 1, ax] measured from the left edge of the image, so its centre sits
## (ax - 0.5) pixels from that edge, and rows are counted the same way from the
## top.  Anything else shifts the image by a whole pixel.
bgimage_anchor_layer <- function(image, pixel, x, y, scale) {
    if (!is.numeric(pixel) || length(pixel) != 2 || anyNA(pixel)) {
        stop("`pixel` should be a numeric vector of length 2, ",
             "c(col, row) counted from the top-left corner of the image")
    }
    ## `Inf` and `NA` both pass a bare `scale <= 0` test and would place an
    ## infinite or missing extent in the panel, so require a finite value.
    if (!is.numeric(scale) || length(scale) != 1 || !is.finite(scale) ||
        scale <= 0) {
        stop("`scale` (data units per image pixel) should be a single positive number")
    }
    ## Likewise for the anchor point: an `Inf`/`NA` coordinate would give the
    ## raster an infinite or missing corner.
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
        stop("`x` should be a single finite number that `pixel` is anchored to")
    }
    if (!is.numeric(y) || length(y) != 1 || !is.finite(y)) {
        stop("`y` should be a single finite number that `pixel` is anchored to")
    }

    image <- bgimage_raster(image)
    info <- magick::image_info(image)
    W <- info$width[[1]]
    H <- info$height[[1]]
    ax <- pixel[[1]]
    ay <- pixel[[2]]
    if (ax < 1 || ax > W || ay < 1 || ay > H) {
        stop("`pixel` is outside the image: valid columns are 1..", W,
             " and rows are 1..", H)
    }

    ggplot2::annotation_raster(
        image,
        xmin = x - (ax - 0.5) * scale,
        xmax = x + (W - ax + 0.5) * scale,
        ymin = y - (H - ay + 0.5) * scale,
        ymax = y + (ay - 0.5) * scale
    )
}
