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

test_that("geom_bgimage uses one isolated row for one-row data", {
    plot <- bgimage_plot(data.frame(x = 0.5, y = 0.5))
    background_layer <- plot$layers[[1L]]

    expect_false(background_layer$inherit.aes)
    expect_equal(nrow(background_layer$data), 1L)
    expect_length(bgimage_raster_grobs(ggplot2::ggplotGrob(plot)), 1L)
})

test_that("geom_bgimage renders one raster for multi-row data", {
    data <- data.frame(x = seq_len(20), y = seq_len(20))
    plot <- bgimage_plot(data)

    expect_length(bgimage_raster_grobs(ggplot2::ggplotGrob(plot)), 1L)
})
