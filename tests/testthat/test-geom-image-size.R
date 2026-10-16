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

test_that("a multi-frame image keeps the aspect of the frame it draws", {
    ## `as.raster()` draws the first frame of a multi-frame image, so the ratio
    ## used to size the grob has to be that frame's. `image_info()` answers one
    ## row per frame, and a vector there breaks `is.finite()` with "the
    ## condition has length > 1" and fails the whole layer.
    first <- magick::image_blank(20, 10, color = "red")
    rest <- magick::image_blank(20, 10, color = "blue")
    two_frames <- magick::image_join(first, rest)

    expect_length(two_frames, 2L)
    expect_identical(ggimage:::getAR2(two_frames), 2)

    common <- list(
        x = 0.5, y = 0.5, size = 0.05, img = two_frames,
        colour = NULL, opacity = 1, angle = 0, adj = 1,
        image_fun = NULL, hjust = 0.5, by = "width",
        asp = 1, use_cache = FALSE
    )
    ## sizing by `size` leaves the width to be resolved at draw time, so what
    ## this branch has to prove is that a scalar ratio no longer breaks it
    expect_no_error(do.call(ggimage:::imageGrob, common))

    ## an explicit width plus the first frame's ratio of 2 gives half the height
    only_width <- do.call(ggimage:::imageGrob,
                          c(common, list(width = 0.2, height = NA_real_)))
    expect_equal(as.numeric(only_width$height), 0.1)
})

test_that("a multi-frame image in a list-column draws without erroring", {
    ## the layer is built per row, so a cell holding more than one frame used
    ## to abort "Problem while converting geom to grob"
    frames <- magick::image_join(magick::image_blank(20, 10, color = "red"),
                                 magick::image_blank(20, 10, color = "blue"))
    d <- data.frame(x = c(0.4, 0.6), y = c(0.5, 0.5))
    d$image <- list(frames)

    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.2) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1))
    expect_no_error(grid::grid.draw(ggplot2::ggplotGrob(p)))
})
