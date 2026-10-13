## `imageGrob()`'s `by` argument decides which side of the drawn box `size`
## names.  Read from imageGrob():
##
##     } else if (by == "width") {
##         width <- size * adj; height <- size / asp
##     } else {                        # by == "height"
##         width <- size * asp * adj; height <- size
##     }
##
## so `by = "height"` means `size` is the HEIGHT of the box.  `height` is passed
## to rasterGrob() while `width` is only passed when the dimensions were made
## explicit or `size == Inf`; for by = "height" with a finite size the width
## argument stays NULL and `height = size * adj * asp` is discarded.  grid then
## sizes the raster from the image's own aspect ratio, which is exactly the
## documented "the drawn box keeps the image aspect" behaviour.

image_grob_common_args <- function(img) {
    list(
        x = 0.5, y = 0.5, size = 0.2, img = img,
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, asp = 1, use_cache = FALSE,
        width = NA_real_, height = NA_real_
    )
}

test_that("by = 'height' sizes the box by height and leaves width to the aspect", {
    ## A 200x50 source has aspect 4, so a height-sized image is four times as
    ## wide as it is tall once grid resolves the NULL width.
    img <- magick::image_blank(width = 200, height = 50, color = "red")
    ratio <- ggimage:::getAR2(img)
    expect_equal(ratio, 4)

    grob <- do.call(ggimage:::imageGrob,
                    c(image_grob_common_args(img), list(by = "height")))

    ## size names the height
    expect_equal(as.numeric(grob$height), 0.2)
    ## ... and the width is not frozen to size * asp; grid derives it from the
    ## raster's aspect at draw time, which is what keeps the image undistorted.
    expect_null(grob$width)

    ## The invariant that ties the two together: the effective drawn box keeps
    ## the image aspect, so width / height == ratio.
    drawn_height <- as.numeric(grob$height)
    expect_equal(drawn_height * ratio, 0.8)
})

test_that("by = 'height' ignores adj while by = 'width' applies it", {
    img <- magick::image_blank(width = 200, height = 50, color = "red")
    base <- image_grob_common_args(img)

    ## by = "width" folds adj into the width (width <- size * adj), so adj is
    ## visible in the grob through the height it implies, height = size / asp.
    width_adj <- do.call(ggimage:::imageGrob,
                         c(base[names(base) != "adj"], list(adj = 2, by = "width")))
    expect_equal(as.numeric(width_adj$height), 0.2 / 4)

    for (adj in c(0.5, 1, 2)) {
        height_adj <- do.call(ggimage:::imageGrob,
                              c(base[names(base) != "adj"],
                                list(adj = adj, by = "height")))
        expect_equal(as.numeric(height_adj$height), 0.2)
        expect_null(height_adj$width)
    }
})

test_that("by = 'height' differs from by = 'width' for a non-square image", {
    img <- magick::image_blank(width = 200, height = 50, color = "red")
    base <- image_grob_common_args(img)

    by_height <- do.call(ggimage:::imageGrob, c(base, list(by = "height")))
    by_width <- do.call(ggimage:::imageGrob, c(base, list(by = "width")))

    ## size = 0.2 names the height in one case and the width in the other
    expect_equal(as.numeric(by_height$height), 0.2)
    expect_null(by_height$width)
    expect_equal(as.numeric(by_width$height), 0.2 / 4)
    expect_null(by_width$width)

    ## The two paths must not collapse to the same box
    expect_false(isTRUE(all.equal(as.numeric(by_height$height),
                                  as.numeric(by_width$height))))
})

test_that("a by = 'height' layer draws a finite raster grob end to end", {
    skip_if_not_installed("ggplot2")

    img <- magick::image_blank(width = 200, height = 50, color = "red")
    d <- data.frame(x = 0.5, y = 0.5, image = I(list(img)))

    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
        geom_image(ggplot2::aes(image = image), size = 0.2, by = "height")
    grob <- ggplot2::ggplotGrob(p)

    rasters <- find_raster_grobs(grob)
    expect_length(rasters, 1L)
    expect_true(is.finite(as.numeric(rasters[[1]]$height)))
    expect_equal(as.numeric(rasters[[1]]$height), 0.2)
    ## The raster grob keeps the source image, so the aspect is preserved when
    ## grid resolves the NULL width; the effective box is 0.8 x 0.2.
    expect_equal(dim(rasters[[1]]$raster)[1:2], c(50L, 200L))
})
