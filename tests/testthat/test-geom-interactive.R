## `geom_image_interactive()` is the only route that draws a per-row magick
## object through ggiraph, and commit 24ebecb ("repel boxes and interactive
## grobs read each row's image") made it read each row with `image_at()`.
## Before that fix every row received the first image of the list column.
## These tests pin the row alignment and the ggiraph attributes, which are the
## two things the interactive path must preserve.

## A one-layer interactive plot over a two-element list column: red then blue.
interactive_image_plot <- function(...) {
    red <- magick::image_blank(width = 4, height = 4, color = "red")
    blue <- magick::image_blank(width = 4, height = 4, color = "blue")
    d <- data.frame(
        x = c(0.25, 0.75),
        y = c(0.5, 0.5),
        image = I(list(red, blue)),
        tooltip = c("red-row", "blue-row"),
        data_id = c("red-row", "blue-row")
    )
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
        geom_image_interactive(
            ggplot2::aes(image = image, tooltip = tooltip, data_id = data_id),
            ...
        )
}

first_pixel <- function(grob) as.character(grob$raster[1L, 1L])

test_that("geom_image_interactive draws each row's own image in row order", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggiraph")

    grob <- ggplot2::ggplotGrob(interactive_image_plot(size = 0.4))
    rasters <- find_raster_grobs(grob)

    expect_length(rasters, 2L)
    ## Row 1 is red and row 2 is blue: a shared or reversed image column would
    ## produce two identical pixels or the pair in the wrong order.
    expect_equal(first_pixel(rasters[[1L]]), "#ff0000ff")
    expect_equal(first_pixel(rasters[[2L]]), "#0000ffff")
    expect_false(identical(first_pixel(rasters[[1L]]), first_pixel(rasters[[2L]])))
})

test_that("geom_image_interactive preserves ggiraph interactive attributes", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggiraph")

    grob <- ggplot2::ggplotGrob(interactive_image_plot(size = 0.4))
    rasters <- find_raster_grobs(grob)
    expect_length(rasters, 2L)

    for (i in seq_along(rasters)) {
        r <- rasters[[i]]
        ## ggiraph marks an interactive grob by adding a class and storing the
        ## parameter names plus their per-row values on the grob itself.  A
        ## geom that dropped them would still draw, but nothing would be
        ## clickable in the HTML output.
        expect_true(any(grepl("interactive", class(r))))

        expect_false(is.null(r$.ipar))
        expect_true(all(c("tooltip", "data_id") %in% r$.ipar))

        expect_false(is.null(r$.interactive))
        expect_true(all(c("tooltip", "data_id") %in% names(r$.interactive)))
    }

    ## The values must follow the rows, not repeat the first row's.
    expect_equal(rasters[[1L]]$.interactive$tooltip, "red-row")
    expect_equal(rasters[[1L]]$.interactive$data_id, "red-row")
    expect_equal(rasters[[2L]]$.interactive$tooltip, "blue-row")
    expect_equal(rasters[[2L]]$.interactive$data_id, "blue-row")
})

test_that("geom_image_interactive falls back to the plain geom without ipar", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggiraph")

    ## No interactive aesthetic is mapped, so `.check_ipar_params()` is FALSE
    ## and draw_panel delegates to GeomImage.  The image must still be drawn.
    red <- magick::image_blank(width = 4, height = 4, color = "red")
    blue <- magick::image_blank(width = 4, height = 4, color = "blue")
    d <- data.frame(x = c(0.25, 0.75), y = c(0.5, 0.5), image = I(list(red, blue)))

    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
        geom_image_interactive(ggplot2::aes(image = image), size = 0.4)
    rasters <- find_raster_grobs(ggplot2::ggplotGrob(p))

    expect_length(rasters, 2L)
    expect_equal(first_pixel(rasters[[1L]]), "#ff0000ff")
    expect_equal(first_pixel(rasters[[2L]]), "#0000ffff")
})

test_that("geom_image_interactive raster grobs are finite and use the size", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("ggiraph")

    rasters <- find_raster_grobs(ggplot2::ggplotGrob(interactive_image_plot(size = 0.4)))
    expect_length(rasters, 2L)
    for (r in rasters) {
        expect_true(is.finite(as.numeric(r$height)))
        expect_equal(as.numeric(r$height), 0.4)
    }
})
