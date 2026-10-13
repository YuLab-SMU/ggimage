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

local_image_file <- function(color, size = 4L) {
    file <- tempfile(fileext = ".png")
    magick::image_write(magick::image_blank(size, size, color = color), file)
    file
}

missing_image_file <- function() {
    file.path(tempdir(), "ggimage-no-such-file.png")
}

## ggimage reports an image it cannot read as a warning about that row, so the
## checks below care about the grobs that survive, not about the noise
build_quietly <- function(plot) {
    suppressWarnings(ggplot2::ggplotGrob(plot))
}

test_that("an unreadable image costs its own row only", {
    good <- local_image_file("red")
    plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = c(good, missing_image_file(), good)))

    expect_no_error(build_quietly(plot))
    ## one broken path must not blank the whole panel
    expect_length(raster_grobs(build_quietly(plot)), 2L)
})

test_that("an absent image draws nothing instead of failing", {
    data <- data.frame(x = 1:2, y = 1:2)
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = rep("", 2L)))

    expect_warning(ggplot2::ggplotGrob(plot), "Invalid image path")
    expect_length(raster_grobs(build_quietly(plot)), 0L)

    ## NA is a missing row for ggplot2 itself
    na_plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = rep(NA_character_, 2L)))
    expect_length(raster_grobs(build_quietly(na_plot)), 0L)
})

test_that("a row whose image cannot be read leaves the plot buildable", {
    plot <- ggplot2::ggplot(data.frame(x = 1L, y = 1L), ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = missing_image_file()))

    expect_no_error(build_quietly(plot))
    expect_length(raster_grobs(build_quietly(plot)), 0L)
})

test_that("a list column of image objects draws one raster per row", {
    data <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5))
    data$image <- list(magick::image_blank(4, 4, color = "red"),
                       magick::image_blank(4, 4, color = "blue"))

    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image), size = 0.4)

    grobs <- raster_grobs(build_quietly(plot))
    expect_length(grobs, 2L)
    expect_equal(unname(vapply(grobs, function(grob) grob$raster[1L, 1L], character(1))),
                 c("#ff0000ff", "#0000ffff"))
})
