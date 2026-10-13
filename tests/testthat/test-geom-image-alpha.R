## A row whose alpha is NA names no opacity: it draws nothing rather than being
## silently promoted to fully opaque, matching the contract of an unreadable
## image and of `size = NA`.

raster_grobs <- function(x) {
    out <- list()
    if (inherits(x, "rastergrob")) out[[length(out) + 1L]] <- x
    for (children in list(x$children, x$grobs)) {
        if (!is.null(children)) {
            for (child in children) out <- c(out, raster_grobs(child))
        }
    }
    out
}

first_image_file <- function(color) {
    file <- tempfile(fileext = ".png")
    magick::image_write(magick::image_blank(8L, 8L, color = color), file, format = "png")
    file
}

built_grobs <- function(plot) {
    suppressWarnings(raster_grobs(ggplot2::ggplotGrob(plot)))
}

test_that("an NA alpha costs its own row only", {
    red <- first_image_file("red")
    blue <- first_image_file("blue")
    data <- data.frame(x = c(0.3, 0.7), y = 0.5,
                      image = c(red, blue), a = c(1, NA))
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    grobs <- built_grobs(plot)
    ## the surviving row is the first one, and keeps its own image
    expect_length(grobs, 1L)
    expect_equal(as.character(grobs[[1L]]$raster[1L, 1L]), "#ff0000ff")
})

test_that("every row NA leaves the plot buildable and empty", {
    data <- data.frame(x = c(0.3, 0.7), y = 0.5,
                       image = c(first_image_file("red"), first_image_file("blue")),
                       a = c(NA_real_, NA_real_))
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    expect_no_error(built_grobs(plot))
    expect_length(built_grobs(plot), 0L)
})

test_that("an NA alpha does not shift the rows after it", {
    ## the batch keeps the skipped slot NULL: a dropped row may not move the
    ## image of a later row up into its place
    data <- data.frame(x = c(0.3, 0.7), y = 0.5, a = c(NA, 1))
    data$image <- list(magick::image_blank(8L, 8L, color = "red"),
                       magick::image_blank(8L, 8L, color = "blue"))
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    grobs <- built_grobs(plot)
    expect_length(grobs, 1L)
    expect_equal(as.character(grobs[[1L]]$raster[1L, 1L]), "#0000ffff")
})

test_that("prepare_image_batch keeps the skipped slot in place", {
    red <- magick::image_blank(8L, 8L, color = "red")
    blue <- magick::image_blank(8L, 8L, color = "blue")
    prepared <- suppressWarnings(
        ggimage:::prepare_image_batch(list(red, blue), opacity = c(1, NA))
    )

    expect_length(prepared, 2L)
    expect_true(inherits(prepared[[1L]], "magick-image"))
    expect_null(prepared[[2L]])
})

test_that("the skipped row is reported, not swallowed", {
    data <- data.frame(x = 1, y = 1,
                       image = first_image_file("red"), a = NA_real_)
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    messages <- character()
    withCallingHandlers(
        ggplot2::ggplotGrob(plot),
        warning = function(w) {
            messages <<- c(messages, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_match(paste(messages, collapse = "\n"), "alpha", ignore.case = TRUE)
})

test_that("a mapped alpha without any NA is unaffected", {
    data <- data.frame(x = c(0.3, 0.7), y = 0.5,
                       image = c(first_image_file("red"), first_image_file("blue")),
                       a = c(1, 0.5))
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
        ggimage::geom_image(ggplot2::aes(image = image, alpha = a), size = 0.4) +
        ggplot2::scale_alpha_identity()

    grobs <- built_grobs(plot)
    expect_length(grobs, 2L)
    ## the second row is the blue image, dimmed by its own alpha (not skipped,
    ## not swapped with row 1); the alpha byte is magick's rounding, not the
    ## point of this test, so only the rgb prefix is asserted
    expect_match(as.character(grobs[[2L]]$raster[1L, 1L]), "^#0000ff")
})
