##' add image as background to plot panel.
##'
##'
##' @title geom_bgimage
##' @param image image file, or a magick image object
##' @param pixel the pixel of `image` to anchor, given as `c(col, row)` counted
##' from the top-left corner of the image, i.e. `col` pixels from the left and
##' `row` pixels from the top. If `NULL` (the default) the image is stretched to
##' fill the plot panel and the remaining parameters are ignored.
##' @param x x coordinate of the data point that `pixel` is anchored to. Only
##' used when `pixel` is given.
##' @param y y coordinate of the data point that `pixel` is anchored to. Only
##' used when `pixel` is given.
##' @param scale number of data units per image pixel. Required when `pixel` is
##' given; it controls the size of the image in data coordinates. To make the
##' image `w` data units wide, use `scale = w / image_info(image)$width`.
##' @return ggplot
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
##' ## bottom-left pixel is anchored to the origin, and `scale` sets the size
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
        x <- y <- NULL
        geom_image(
            data = data.frame(x = 0.5, y = 0.5),
            mapping = ggplot2::aes(x = x, y = y),
            inherit.aes = FALSE,
            image = object$image,
            size = Inf
        )
    } else {
        bgimage_anchor_layer(object$image, object$pixel, object$x, object$y,
                             object$scale)
    }
    plot$layers <- c(list(background_layer), plot$layers)
    return(plot)
}

## Place `image` in data coordinates so that its `pixel` lands on `(x, y)`.
##
## A pixel is a cell, not a grid line: pixel column `ax` covers the interval
## [ax - 1, ax] on the raster grid, so its centre sits (ax - 0.5) from the left
## edge of the image (and likewise for rows, counted from the top).  That is
## where the `+ 0.5` below comes from -- using `ax` instead would place the
## anchor half a pixel off.
bgimage_anchor_layer <- function(image, pixel, x, y, scale) {
    if (!is.numeric(pixel) || length(pixel) != 2 || anyNA(pixel)) {
        stop("`pixel` should be a numeric vector of length 2, ",
             "c(col, row) counted from the top-left corner of the image")
    }
    if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale <= 0) {
        stop("`scale` (data units per image pixel) should be a single positive number")
    }

    if (is.character(image)) {
        image <- image_read(image)
    }
    info <- image_info(image)
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
        xmin = x - (ax + 0.5) * scale,
        xmax = x + (W - ax - 0.5) * scale,
        ymin = y - (H - ay - 0.5) * scale,
        ymax = y + (ay + 0.5) * scale
    )
}
