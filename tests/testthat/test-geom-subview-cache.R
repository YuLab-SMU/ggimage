subview_panel_rects <- function(plot) {
    grob <- ggplot2::ggplotGrob(plot)
    panel <- grob$grobs[[which(grob$layout$name == "panel")]]
    children <- lapply(seq_along(panel$children), function(i) panel$children[[i]])
    Filter(function(child) inherits(child, "rect") && !is.null(child$gp$fill), children)
}

test_that("subview conversion memo reuses repeated grobs", {
    calls <- 0L
    converter <- function(x) {
        calls <<- calls + 1L
        x
    }
    memo <- ggimage:::.subview_grob_memo(converter)
    repeated <- grid::rectGrob(gp = grid::gpar(fill = "red"))

    converted <- lapply(list(repeated, repeated, repeated), memo)

    expect_equal(calls, 1L)
    expect_identical(converted[[1L]], converted[[2L]])
    expect_identical(converted[[2L]], converted[[3L]])
})

test_that("subview conversion memo reuses repeated local images", {
    calls <- 0L
    converter <- function(x) {
        calls <<- calls + 1L
        ggplotify::as.grob(x)
    }
    memo <- ggimage:::.subview_grob_memo(converter)
    image <- magick::image_blank(width = 2, height = 2, color = "red")

    converted <- lapply(list(image, image), memo)

    expect_equal(calls, 1L)
    expect_s3_class(converted[[1L]], "rastergrob")
    expect_identical(converted[[1L]], converted[[2L]])
})

test_that("subview conversion memo keeps unique grobs distinct", {
    calls <- 0L
    converter <- function(x) {
        calls <<- calls + 1L
        x
    }
    memo <- ggimage:::.subview_grob_memo(converter)
    subviews <- list(
        grid::rectGrob(gp = grid::gpar(fill = "red")),
        grid::rectGrob(gp = grid::gpar(fill = "blue")),
        grid::rectGrob(gp = grid::gpar(fill = "green"))
    )

    converted <- lapply(subviews, memo)

    expect_equal(calls, length(subviews))
    expect_equal(
        vapply(converted, function(x) as.character(x$gp$fill), character(1)),
        c("red", "blue", "green")
    )
})

test_that("repeated subviews preserve per-row extents and visual order", {
    repeated <- grid::rectGrob(gp = grid::gpar(fill = "red"))
    x <- c(0.2, 0.5, 0.8)
    y <- c(0.2, 0.7, 0.4)
    plot <- ggplot2::ggplot() +
        ggimage::geom_subview(
            x = x, y = y, width = 0.2, height = 0.1,
            subview = list(repeated, repeated, repeated)
        ) +
        ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)

    layers <- plot$layers
    rects <- subview_panel_rects(plot)

    expect_length(layers, 3L)
    expect_equal(unname(vapply(layers, function(layer) layer$geom_params$xmin, numeric(1))),
                 x - 0.1)
    expect_equal(unname(vapply(layers, function(layer) layer$geom_params$xmax, numeric(1))),
                 x + 0.1)
    expect_equal(unname(vapply(layers, function(layer) layer$geom_params$ymin, numeric(1))),
                 y - 0.05)
    expect_equal(unname(vapply(layers, function(layer) layer$geom_params$ymax, numeric(1))),
                 y + 0.05)
    expect_length(rects, 3L)
    expect_equal(vapply(rects, function(x) as.character(x$gp$fill), character(1)),
                 rep("red", 3L))
})

test_that("unique subviews preserve grob count and visual structure", {
    subviews <- list(
        grid::rectGrob(gp = grid::gpar(fill = "red")),
        grid::rectGrob(gp = grid::gpar(fill = "blue")),
        grid::rectGrob(gp = grid::gpar(fill = "green"))
    )
    plot <- ggplot2::ggplot() +
        ggimage::geom_subview(
            x = c(0.2, 0.5, 0.8), y = c(0.2, 0.7, 0.4),
            width = 0.2, height = 0.1, subview = subviews
        ) +
        ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)

    rects <- subview_panel_rects(plot)

    expect_length(rects, length(subviews))
    expect_equal(vapply(rects, function(x) as.character(x$gp$fill), character(1)),
                 c("red", "blue", "green"))
})
