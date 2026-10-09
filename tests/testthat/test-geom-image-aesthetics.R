## Issue #4 asked for `colour` and `alpha` to be settable in aes(). Both are
## implemented through GeomImage$default_aes, but neither page documented it, so
## the supported set is asserted against the code here: editing one without the
## other fails the suite.

test_that("the documented aesthetics are the ones the geom implements", {
    for (topic in c("geom_image", "geom_image_repel")) {
        rd <- pkg_rd_file(topic)
        if (is.null(rd)) skip("man/ not available")
        documented <- rd_aesthetics(rd)
        expect_false(is.null(documented), info = topic)
        expect_setequal(documented$required, ggimage:::GeomImage$required_aes)
        expect_setequal(documented$optional, names(ggimage:::GeomImage$default_aes))
    }
})

test_that("colour and alpha are mapped per row onto the drawn image", {
    img <- system.file("extdata/Rlogo.png", package = "ggimage")
    d <- data.frame(x = c(0.25, 0.75), y = 0.5,
                    a = c(1, 0.5), g = factor(c("a", "b")))

    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = img,
                                         colour = g, alpha = a)) +
        geom_image(width = 0.3) +
        ggplot2::scale_colour_manual(values = c(a = "red", b = "blue")) +
        ## the default continuous alpha scale would map the range onto
        ## [0.1, 1]; identity keeps the values as written
        ggplot2::scale_alpha_identity() +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::theme_void()

    grobs <- repel_grobs(ggplot2::ggplotGrob(p))
    expect_length(grobs, 2L)

    rgba <- lapply(grobs, function(g) grDevices::col2rgb(as.matrix(g$raster),
                                                         alpha = TRUE))
    alpha <- vapply(rgba, function(x) mean(x[4, ]) / 255, numeric(1))
    ## the R logo is a circle, so its mean alpha over the bounding box is below
    ## 1; opacity = 1 must reproduce that baseline unchanged
    baseline <- mean(as.integer(
        magick::image_data(magick::image_read(img), channels = "rgba")[4, , ]
    )) / 255
    expect_equal(alpha[1], baseline, tolerance = 0.02)
    expect_equal(alpha[2] / alpha[1], 0.5, tolerance = 0.05)

    rgb <- lapply(rgba, function(x) rowMeans(x[1:3, , drop = FALSE]))
    expect_gt(rgb[[1]][1] - rgb[[1]][3], 50)   # first row tinted red
    expect_gt(rgb[[2]][3] - rgb[[2]][1], 50)   # second row tinted blue
})
