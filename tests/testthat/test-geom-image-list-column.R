## The `image` aesthetic may be mapped from a data-frame list column, which
## holds one image object per row.  These checks cover the boundaries the
## end-to-end draw test and the cache-key test do not: what prepare_image_batch
## returns per row, both cache modes over a column with repeated objects, row-wise
## aesthetic alignment, and rows whose image is a bad path or NULL.

raster_grobs <- function(x) {
    out <- list()
    if (inherits(x, "rastergrob")) out[[length(out) + 1L]] <- x
    children <- list(x$children, x$grobs)
    for (items in children) {
        if (!is.null(items)) {
            for (child in items) out <- c(out, raster_grobs(child))
        }
    }
    out
}

## The first pixel of each raster, in draw order -- rgb hex, so the alpha byte is
## part of the string and can be compared without rounding to a literal.
raster_pixels_from <- function(grob) {
    grobs <- raster_grobs(grob)
    unname(vapply(grobs, function(g) as.character(g$raster[1L, 1L]), character(1)))
}

## ggimage reports an image it cannot read as a warning about that row, so the
## checks below care about the grobs that survive, not about the noise.
build_quietly <- function(plot) {
    suppressWarnings(ggplot2::ggplotGrob(plot))
}

raster_pixels <- function(plot) {
    raster_pixels_from(build_quietly(plot))
}

missing_image_file <- function() {
    file.path(tempdir(), "ggimage-no-such-file.png")
}

test_that("prepare_image_batch returns the image object itself, not a wrapper", {
    red <- magick::image_blank(8L, 8L, color = "red")
    blue <- magick::image_blank(8L, 8L, color = "blue")

    prepared <- ggimage:::prepare_image_batch(list(red, blue), colour = NULL,
                                              opacity = 1, angle = 0)

    expect_length(prepared, 2L)
    ## `[[i]]` must hand back the object; a one-element list wrapper would draw
    ## nothing (imageGrob() would not see a magick-image) and would also make the
    ## two rows compare equal below
    for (i in seq_along(prepared)) {
        expect_true(inherits(prepared[[i]], "magick-image"))
        expect_false(is.list(prepared[[i]]))
    }

    red_pixel <- as.integer(magick::image_data(prepared[[1L]], channels = "rgba")[1:3, 1L, 1L])
    blue_pixel <- as.integer(magick::image_data(prepared[[2L]], channels = "rgba")[1:3, 1L, 1L])
    expect_false(identical(red_pixel, blue_pixel))
    expect_equal(red_pixel, c(255L, 0L, 0L))
    expect_equal(blue_pixel, c(0L, 0L, 255L))
})

test_that("a list column with duplicate rows keeps row order under both cache modes", {
    data <- data.frame(x = c(0.3, 0.5, 0.7), y = c(0.5, 0.5, 0.5))
    data$image <- list(magick::image_blank(8L, 8L, color = "red"),
                       magick::image_blank(8L, 8L, color = "red"),
                       magick::image_blank(8L, 8L, color = "blue"))

    ## the repeated first two rows must share one transform, not collapse to one
    ## drawn row or hand the third row's image to a later row
    cached <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image), size = 0.4, use_cache = TRUE)
    uncached <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image), size = 0.4, use_cache = FALSE)

    expected <- c("#ff0000ff", "#ff0000ff", "#0000ffff")
    expect_equal(raster_pixels(cached), expected)
    expect_equal(raster_pixels(uncached), expected)
})

test_that("per-row alpha over a list column is applied to that row's image", {
    data <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5), a = c(1, 0.5))
    data$image <- list(magick::image_blank(8L, 8L, color = "red"),
                       magick::image_blank(8L, 8L, color = "red"))

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    pixels <- raster_pixels(plot)
    ## both rows draw, and the row-wise value reaches the raster
    expect_length(pixels, 2L)

    alpha_byte <- function(pixel) {
        strtoi(substr(pixel, 8L, 9L), base = 16L)
    }
    expect_lt(alpha_byte(pixels[2L]), alpha_byte(pixels[1L]))
})

test_that("per-row angle over a list column rotates that row's image only", {
    data <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5), ang = c(0, 90))
    data$image <- list(magick::image_blank(8L, 4L, color = "red"),
                       magick::image_blank(8L, 4L, color = "red"))

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, angle = ang), size = 0.4)

    grobs <- raster_grobs(build_quietly(plot))
    expect_length(grobs, 2L)
    dims <- lapply(grobs, function(grob) dim(grob$raster))
    ## a 90-degree turn swaps the raster's dimensions; an unrotated sibling keeps
    ## the source 8x4 orientation
    expect_false(identical(dims[[1L]], dims[[2L]]))
    expect_equal(dims[[1L]], rev(dims[[2L]]))
})

test_that("a list column row with a bad path warns and costs only that row", {
    good <- magick::image_blank(8L, 8L, color = "red")
    data <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5))
    data$image <- list(good, missing_image_file())

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image), size = 0.4)

    ## the bad path is reported as a warning, not an error, and only its own row
    ## is lost
    grob <- suppressWarnings(ggplot2::ggplotGrob(plot))
    expect_warning(ggplot2::ggplotGrob(plot))

    pixels <- raster_pixels_from(grob)
    expect_length(pixels, 1L)
    expect_equal(pixels, "#ff0000ff")
})

test_that("a list column row holding NULL leaves the other row drawable", {
    good <- magick::image_blank(8L, 8L, color = "red")
    data <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5))
    data$image <- list(good, NULL)

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image), size = 0.4)

    ## ggplot2 drops the NULL row itself, so the build warns about the removed
    ## row rather than raising
    grob <- suppressWarnings(ggplot2::ggplotGrob(plot))

    pixels <- raster_pixels_from(grob)
    expect_length(pixels, 1L)
    expect_equal(pixels, "#ff0000ff")
})
