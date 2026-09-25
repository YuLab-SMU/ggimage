test_that("explicit width and height control image dimensions", {
    img <- system.file("extdata/Rlogo.png", package = "ggimage")
    common <- list(
        x = 0.5, y = 0.5, size = 0.05, img = img,
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, by = "width",
        asp = 1, use_cache = FALSE
    )

    both <- do.call(ggimage:::imageGrob, c(common, list(width = 0.2, height = 0.3)))
    expect_equal(as.numeric(both$width), 0.2)
    expect_equal(as.numeric(both$height), 0.3)

    ratio <- ggimage:::getAR2(magick::image_read(img))
    only_width <- do.call(ggimage:::imageGrob, c(common, list(width = 0.2, height = NA_real_)))
    expect_equal(as.numeric(only_width$width), 0.2)
    expect_equal(as.numeric(only_width$height), 0.2 / ratio)

    only_height <- do.call(ggimage:::imageGrob, c(common, list(width = NA_real_, height = 0.3)))
    expect_equal(as.numeric(only_height$width), 0.3 * ratio)
    expect_equal(as.numeric(only_height$height), 0.3)
})

test_that("invalid explicit dimensions fall back to size and by", {
    img <- system.file("extdata/Rlogo.png", package = "ggimage")
    common <- list(
        x = 0.5, y = 0.5, size = 0.05, img = img,
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, by = "width",
        asp = 1, use_cache = FALSE
    )
    baseline <- do.call(ggimage:::imageGrob, c(common, list(width = NA_real_, height = NA_real_)))
    invalid <- do.call(ggimage:::imageGrob, c(common, list(width = 0, height = -1)))

    expect_equal(invalid$width, baseline$width)
    expect_equal(invalid$height, baseline$height)
})

test_that("width and height are recognized aesthetics", {
    expect_true(all(c("width", "height") %in% names(ggimage:::GeomImage$default_aes)))

    d <- data.frame(
        x = c(0.25, 0.75), y = c(0.25, 0.75),
        image = system.file("extdata/Rlogo.png", package = "ggimage"),
        width = c(0.1, 0.2), height = c(0.15, 0.25)
    )
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image,
                                         width = width, height = height)) +
        geom_image()
    built <- ggplot2::ggplot_build(p)
    layer_data <- built$data[[1]]
    expect_equal(layer_data$width, d$width)
    expect_equal(layer_data$height, d$height)
})
