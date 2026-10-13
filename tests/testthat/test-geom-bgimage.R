bgimage_raster_grobs <- function(x) {
    out <- list()
    if (inherits(x, "rastergrob")) out[[length(out) + 1L]] <- x
    children <- list(x$children, x$grobs)
    for (items in children) {
        if (!is.null(items)) {
            for (child in items) out <- c(out, bgimage_raster_grobs(child))
        }
    }
    out
}

bgimage_plot <- function(data) {
    ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_bgimage(system.file("extdata/Rlogo.png", package = "ggimage"))
}

bgimage_ranges <- function(plot) {
    panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
    list(x = panel$x.range, y = panel$y.range)
}

test_that("geom_bgimage() does not train the panel scales", {
    data <- data.frame(x = seq(2, 20, by = 2), y = seq_len(10), g = factor(letters[1:10]))
    img <- system.file("extdata/Rlogo.png", package = "ggimage")

    continuous <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point()
    discrete <- ggplot2::ggplot(data, ggplot2::aes(g, y)) + ggplot2::geom_point()

    ## a background must be decoration: it may not contribute a point of its
    ## own to the scales, because a dummy row also breaks a discrete axis
    expect_equal(bgimage_ranges(continuous + ggimage::geom_bgimage(img)),
                 bgimage_ranges(continuous))
    expect_equal(bgimage_ranges(discrete + ggimage::geom_bgimage(img)),
                 bgimage_ranges(discrete))
})

test_that("geom_bgimage() renders one raster for any number of rows", {
    expect_length(bgimage_raster_grobs(ggplot2::ggplotGrob(
        bgimage_plot(data.frame(x = 0.5, y = 0.5)))), 1L)
    expect_length(bgimage_raster_grobs(ggplot2::ggplotGrob(
        bgimage_plot(data.frame(x = seq_len(20), y = seq_len(20))))), 1L)
})

## ---- anchored placement ----------------------------------------------------

bgimage_test_image <- function() {
    system.file("extdata/Rlogo.png", package = "ggimage")
}

bgimage_test_size <- function(img = bgimage_test_image()) {
    info <- magick::image_info(magick::image_read(img))
    c(width = info$width[[1L]], height = info$height[[1L]])
}

bgimage_anchor_plot <- function(...) {
    ggplot2::ggplot() + ggimage::geom_bgimage(...)
}

bgimage_anchor_box <- function(...) {
    bgimage_anchor_plot(...)$layers[[1L]]$geom_params[c("xmin", "xmax", "ymin", "ymax")]
}

bgimage_centre <- function(box, pixel, scale) {
    ## pixel (column, row) is 1-based and rows count downwards from the top
    unname(c(x = box[["xmin"]] + (pixel[1L] - 0.5) * scale,
             y = box[["ymax"]] - (pixel[2L] - 0.5) * scale))
}

test_that("geom_bgimage() anchors a chosen pixel to a chosen data point", {
    img <- bgimage_test_image()
    size <- bgimage_test_size(img)
    scale <- 0.01

    box <- bgimage_anchor_box(img, c(1, 1), 0, 0, scale)

    ## the anchored pixel keeps half a pixel of image on its far side, so the
    ## image starts half a pixel before the anchor and ends (width - 0.5) after
    expect_equal(box[["xmin"]], -0.5 * scale)
    expect_equal(box[["xmax"]], (size[["width"]] - 0.5) * scale)
    expect_equal(box[["ymin"]], -(size[["height"]] - 0.5) * scale)
    expect_equal(box[["ymax"]], 0.5 * scale)

    ## the box is in data units, so it must follow the anchor
    moved <- bgimage_anchor_box(img, c(1, 1), 5, -3, scale)
    expect_equal(moved[["xmin"]] - box[["xmin"]], 5)
    expect_equal(moved[["ymax"]] - box[["ymax"]], -3)

    ## and scale is data units per pixel
    bigger <- bgimage_anchor_box(img, c(1, 1), 0, 0, 0.02)
    expect_equal(bigger[["xmax"]] - bigger[["xmin"]], 2 * (box[["xmax"]] - box[["xmin"]]))

    ## the image always covers exactly width * height data units
    expect_equal(box[["xmax"]] - box[["xmin"]], size[["width"]] * scale)
    expect_equal(box[["ymax"]] - box[["ymin"]], size[["height"]] * scale)
})

test_that("the anchored pixel round-trips to the requested data point", {
    img <- bgimage_test_image()
    size <- bgimage_test_size(img)

    pixels <- list(c(1, 1), c(1, size[["height"]]), c(size[["width"]], 1),
                   c(size[["width"]], size[["height"]]),
                   c(size[["width"]] / 2, size[["height"]] / 2), c(17, 203))
    for (pixel in pixels) {
        at <- c(3.5, -2)
        scale <- 0.02
        box <- bgimage_anchor_box(img, pixel, at[1L], at[2L], scale)

        ## the centre of the anchor pixel must be exactly on `at`
        expect_equal(bgimage_centre(box, pixel, scale), at, tolerance = 1e-12)
    }
})

test_that("geom_bgimage() prepends the anchored raster layer", {
    plot <- ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Petal.Width)) +
        ggplot2::geom_point() +
        ggimage::geom_bgimage(bgimage_test_image(), pixel = c(1, 1),
                              x = 0, y = 0, scale = 0.01)

    expect_identical(class(plot$layers[[1L]]$geom)[1L], "GeomRasterAnn")
    expect_identical(class(plot$layers[[2L]]$geom)[1L], "GeomPoint")
})

test_that("geom_bgimage() still fills the panel when pixel is NULL", {
    data <- data.frame(x = seq(2, 20, by = 2), y = seq_len(10))
    img <- bgimage_test_image()

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        ggimage::geom_bgimage(img)

    ## behind the data, one raster, and it does not move the axes
    expect_identical(class(plot$layers[[1L]]$geom)[1L], "GeomRasterAnn")
    expect_length(bgimage_raster_grobs(ggplot2::ggplotGrob(plot)), 1L)
    expect_equal(bgimage_ranges(plot),
                 bgimage_ranges(ggplot2::ggplot(data, ggplot2::aes(x, y)) +
                                    ggplot2::geom_point()))

    ## the panel corners, which is what npc = c(0, 0) and c(1, 1) mean
    box <- plot$layers[[1L]]$geom_params[c("xmin", "xmax", "ymin", "ymax")]
    expect_equal(unname(unlist(box)), c(-Inf, Inf, -Inf, Inf))
})

test_that("geom_bgimage() validates the anchoring arguments", {
    img <- bgimage_test_image()
    size <- bgimage_test_size(img)

    expect_error(bgimage_anchor_plot(img, pixel = 1, scale = 0.01), "length 2")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2, 3), scale = 0.01), "length 2")
    expect_error(bgimage_anchor_plot(img, pixel = c("a", "b"), scale = 0.01), "length 2")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, NA), scale = 0.01), "length 2")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2)), "scale")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = -1), "scale")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = 0), "scale")
    ## an infinite or missing scale would place an infinite extent in the panel
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = Inf), "scale")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = -Inf), "scale")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = NA_real_), "scale")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), scale = NaN), "scale")
    ## ... and so would a non-finite anchor point
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), x = NA_real_, scale = 0.01), "`x`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), x = Inf, scale = 0.01), "`x`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), x = NaN, scale = 0.01), "`x`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), y = NA_real_, scale = 0.01), "`y`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), y = Inf, scale = 0.01), "`y`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), y = -Inf, scale = 0.01), "`y`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), x = c(0, 1), scale = 0.01), "`x`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), y = c(0, 1), scale = 0.01), "`y`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), x = "a", scale = 0.01), "`x`")
    expect_error(bgimage_anchor_plot(img, pixel = c(1, 2), y = "a", scale = 0.01), "`y`")
    expect_error(bgimage_anchor_plot(img, pixel = c(size[["width"]] + 1, 2), scale = 0.01),
                 "outside the image")
    expect_error(bgimage_anchor_plot(img, pixel = c(2, size[["height"]] + 1), scale = 0.01),
                 "outside the image")

    ## the four corners are inside the image
    expect_no_error(bgimage_anchor_plot(img, pixel = c(1, 1), scale = 0.01))
    expect_no_error(bgimage_anchor_plot(img, pixel = c(size[["width"]], size[["height"]]),
                                        scale = 0.01))
    ## a magick image object works as well as a file name
    expect_no_error(bgimage_anchor_plot(magick::image_read(img), pixel = c(5, 5),
                                        scale = 0.01))
})

test_that("geom_bgimage() renders the anchored pixel on the requested data point", {
    ## a featureless image, so its edges can be located unambiguously
    width <- 10L; height <- 8L
    solid <- tempfile(fileext = ".png")
    magick::image_write(magick::image_blank(width, height, color = "black"), solid)

    ## a large scale, so that the half-pixel convention is far bigger than the
    ## measurement noise: anchoring on the pixel's corner instead of its centre
    ## would move the image by 0.5 * scale = 0.2 data units, i.e. about 10
    ## device pixels
    scale <- 0.4                      # data units per pixel
    at <- c(1, -1)                    # anchor target in data units
    pixel <- c(1, 1)                  # the image's top-left pixel
    refs <- data.frame(x = c(6, -2), y = c(2, -3))   # calibration points

    plot <- ggplot2::ggplot() +
        ggimage::geom_bgimage(solid, pixel = pixel, x = at[1L], y = at[2L],
                              scale = scale) +
        ggplot2::geom_point(data = refs, ggplot2::aes(x, y), colour = "#00A000",
                            size = 1.6) +
        ggplot2::annotate("point", x = at[1L], y = at[2L], colour = "#FF00FF",
                          size = 2.6) +
        ggplot2::coord_fixed() +
        ggplot2::scale_x_continuous(limits = c(-3, 7), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(-4, 3), expand = c(0, 0)) +
        ggplot2::theme_void()

    file <- tempfile(fileext = ".png")
    ggplot2::ggsave(file, plot, width = 5, height = 5, dpi = 100, bg = "white")

    pixels <- as.matrix(as.raster(magick::image_read(file)))
    cc <- grDevices::col2rgb(pixels, alpha = FALSE)
    locate <- function(keep) {
        k <- which(keep)
        data.frame(x = ((k - 1) %/% nrow(pixels)) + 1,
                   y = ((k - 1) %% nrow(pixels)) + 1)
    }
    green <- locate(cc[2, ] > 130 & cc[1, ] < 100 & cc[3, ] < 100)
    magenta <- locate(cc[1, ] > 180 & cc[2, ] < 80 & cc[3, ] > 180)
    black <- locate(cc[1, ] < 60 & cc[2, ] < 60 & cc[3, ] < 60)

    ## calibrate page pixels against data units with the two green markers
    low <- green[green$x < mean(range(green$x)), ]
    high <- green[green$x > mean(range(green$x)), ]
    ref_low <- c(x = mean(low$x), y = mean(low$y))
    ref_high <- c(x = mean(high$x), y = mean(high$y))
    to_data <- function(pt) {
        c(x = -2 + (pt[1L] - ref_low[1L]) * 8 / (ref_high[1L] - ref_low[1L]),
          y = -3 + (ref_low[2L] - pt[2L]) * 5 / (ref_low[2L] - ref_high[2L]))
    }

    ## the marker drawn at the anchor target must be at the anchor target
    expect_equal(unname(to_data(c(x = mean(magenta$x), y = mean(magenta$y)))),
                 at, tolerance = 0.04)

    ## the drawn image spans the box around the anchor pixel, i.e. it starts
    ## half a pixel before the anchor pixel's centre and ends half a pixel
    ## after the last pixel's centre
    top_left <- to_data(c(x = min(black$x), y = min(black$y)))
    expect_equal(unname(top_left),
                 c(at[1L] - (pixel[1L] - 0.5) * scale,
                   at[2L] + (pixel[2L] - 0.5) * scale),
                 tolerance = 0.08)

    bottom_right <- to_data(c(x = max(black$x), y = max(black$y)))
    expect_equal(unname(bottom_right),
                 c(at[1L] + (width - pixel[1L] + 0.5) * scale,
                   at[2L] - (height - pixel[2L] + 0.5) * scale),
                 tolerance = 0.08)
})
