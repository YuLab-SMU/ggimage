## ---------------------------------------------------------------------------
## Invariants of the *existing* image geometry that a repulsion layout has to
## reuse. These run today and are the foundation the geom_image_repel()
## contract in test-geom-image-repel.R is built on:
##
##   1. the repulsion box of a row is exactly the box that row is drawn in
##      (same width/height derivation as imageGrob),
##   2. per-row width/height follow the documented recycling + validation
##      rules of recycle_image_dimension(),
##   3. geom_image() itself never separates overlapping images, so any
##      separation a user sees has to come from the repel layer.
## ---------------------------------------------------------------------------

test_that("per-row dimension validation matches the documented rules", {
    recycle <- ggimage:::recycle_image_dimension

    expect_equal(recycle(NULL, 3), rep(NA_real_, 3))
    expect_equal(recycle(numeric(), 2), rep(NA_real_, 2))
    expect_equal(recycle(0.2, 3), rep(0.2, 3))
    expect_equal(recycle(c(0.1, 0.2), 3), c(0.1, 0.2, 0.1))
    ## NA, non-finite and non-positive values are dropped to NA, which routes
    ## the row back to the size/by behaviour
    expect_equal(recycle(c(0.1, NA_real_, 0, -1, Inf, NaN), 6),
                 c(0.1, rep(NA_real_, 5)))
})

test_that("the drawn box is deterministic for a given width/height pair", {
    common <- list(
        x = 0.5, y = 0.5, size = 0.05, img = repel_image_file(),
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, by = "width",
        asp = 1, use_cache = FALSE
    )
    ratio <- ggimage:::getAR2(magick::image_read(repel_image_file()))

    both <- do.call(ggimage:::imageGrob, c(common, list(width = 0.2, height = 0.3)))
    expect_equal(as.numeric(both$width), 0.2)
    expect_equal(as.numeric(both$height), 0.3)

    one_sided <- do.call(ggimage:::imageGrob,
                         c(common, list(width = 0.2, height = NA_real_)))
    expect_equal(as.numeric(one_sided$width), 0.2)
    expect_equal(as.numeric(one_sided$height), 0.2 / ratio)

    other_side <- do.call(ggimage:::imageGrob,
                          c(common, list(width = NA_real_, height = 0.3)))
    expect_equal(as.numeric(other_side$width), 0.3 * ratio)
    expect_equal(as.numeric(other_side$height), 0.3)
})

test_that("imageGrob is a pure function of its anchor and box", {
    base <- list(
        y = 0.5, size = 0.05, img = repel_image_file(),
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, by = "width",
        asp = 1, use_cache = FALSE, width = 0.2, height = 0.1
    )
    at_left <- do.call(ggimage:::imageGrob, c(base, list(x = 0.2)))
    at_right <- do.call(ggimage:::imageGrob, c(base, list(x = 0.8)))

    ## translating the anchor translates the box, and never resizes it
    expect_equal(as.numeric(at_right$x) - as.numeric(at_left$x), 0.6)
    expect_equal(as.numeric(at_right$width), as.numeric(at_left$width))
    expect_equal(as.numeric(at_right$height), as.numeric(at_left$height))

    ## the anchor only moves y; x stays where it was put
    low <- base
    low$x <- 0.2
    low$y <- 0.2
    low <- do.call(ggimage:::imageGrob, low)
    expect_equal(as.numeric(low$y), 0.2)
    expect_equal(as.numeric(low$x), 0.2)
})

test_that("size = Inf collapses every image onto the panel centre", {
    ## geom_image() has no repulsion, so two size = Inf images end up on top of
    ## each other; a repel layer must not produce a non-finite or crashing
    ## layout from this input, because the images have no distinct position to
    ## resolve against
    d <- repel_data(x = c(0.2, 0.8), y = c(0.2, 0.8))
    p <- repel_plot(d, geom_image(size = Inf, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), 2L)
    expect_equal(layout$x, c(0.5, 0.5))
    expect_equal(layout$y, c(0.5, 0.5))
    expect_equal(layout$width, c(1, 1))
    expect_equal(layout$height, c(1, 1))
    expect_equal(repel_overlap_count(layout), 1L)
})

test_that("nudge happens in data space, before the panel transform", {
    d <- repel_data(x = c(0.4), y = c(0.4), width = 0.1, height = 0.1)
    plain <- repel_layout_of(repel_plot(
        d, geom_image(width = 0.1, height = 0.1, use_cache = FALSE)))
    nudged <- repel_layout_of(repel_plot(
        d, geom_image(width = 0.1, height = 0.1, nudge_x = 0.1,
                      nudge_y = 0.1, use_cache = FALSE)))

    ## nudge_x/nudge_y are added in data units and the square panel maps them
    ## one-to-one, so a repel layer that runs after the nudge sees a shifted
    ## but otherwise identical layout
    expect_equal(nudged$x - plain$x, 0.1, tolerance = 1e-6)
    expect_equal(nudged$y - plain$y, 0.1, tolerance = 1e-6)
    expect_equal(nudged$width, plain$width)
})

test_that("geom_image applies per-row width and height without moving anything", {
    d <- repel_data(x = rep(0.5, 3), y = rep(0.5, 3),
                    width = c(0.1, 0.2, 0.3), height = c(0.3, 0.2, 0.1))
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image,
                                         width = width, height = height)) +
        geom_image(use_cache = FALSE) +
        repel_square_scales()
    layout <- repel_layout_of(p)

    ## the baseline: three fully overlapping images, all still at (0.5, 0.5)
    expect_equal(layout$x, rep(0.5, 3))
    expect_equal(layout$y, rep(0.5, 3))
    expect_equal(layout$width, d$width)
    expect_equal(layout$height, d$height)
    expect_equal(repel_overlap_count(layout), 3L)
})

test_that("explicit boxes survive coord_fixed unchanged in native units", {
    d <- repel_data(x = 0.5, y = 0.5, width = 0.2, height = 0.1)
    plain <- repel_plot(d, geom_image(width = 0.2, height = 0.1, use_cache = FALSE))
    fixed <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.2, height = 0.1, use_cache = FALSE) +
        repel_square_scales() +
        ggplot2::coord_fixed()

    expect_equal(repel_layout_of(fixed), repel_layout_of(plain))
})

test_that("overlapping images are dropped or kept identically for NA handling", {
    d <- repel_data(x = c(0.5, NA_real_, 0.5), y = c(0.5, 0.5, NA_real_),
                    width = 0.2, height = 0.2)
    p <- suppressWarnings(repel_plot(d, geom_image(width = 0.2, height = 0.2,
                                                   use_cache = FALSE)))
    layout <- suppressWarnings(repel_layout_of(p))

    ## only the complete row survives, which is what the repel layer must keep
    expect_equal(nrow(layout), 1L)
    expect_equal(layout$x, 0.5)
    expect_equal(layout$y, 0.5)
})
