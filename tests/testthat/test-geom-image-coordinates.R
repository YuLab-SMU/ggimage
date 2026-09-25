coord_image_grobs <- function(x, cls = "rastergrob") {
    out <- list()
    if (inherits(x, cls)) out[[length(out) + 1L]] <- x
    children <- list(x$children, x$grobs)
    for (items in children) {
        if (!is.null(items)) {
            for (child in items) out <- c(out, coord_image_grobs(child, cls))
        }
    }
    out
}

local_image <- system.file("extdata/Rlogo.png", package = "ggimage")

coord_base_args <- list(
    size = 0.05, img = local_image, colour = NULL, opacity = 1,
    angle = 0, adj = 1, image_fun = NULL, by = "width",
    asp = 1, use_cache = FALSE
)

test_that("hjust moves explicit image boxes around their anchor", {
    centered <- do.call(ggimage:::imageGrob, c(
        list(x = 0.5, y = 0.5, hjust = 0.5, width = 0.2, height = 0.1),
        coord_base_args
    ))
    left <- do.call(ggimage:::imageGrob, c(
        list(x = 0.5, y = 0.5, hjust = 0, width = 0.2, height = 0.1),
        coord_base_args
    ))
    right <- do.call(ggimage:::imageGrob, c(
        list(x = 0.5, y = 0.5, hjust = 1, width = 0.2, height = 0.1),
        coord_base_args
    ))

    expect_equal(as.numeric(centered$x), 0.5)
    expect_equal(as.numeric(left$x), 0.6)
    expect_equal(as.numeric(right$x), 0.4)
    expect_equal(as.numeric(left$width), 0.2)
    expect_equal(as.numeric(left$height), 0.1)
})

test_that("nudge_x and nudge_y are applied before coordinate transformation", {
    d <- data.frame(x = 0.5, y = 0.5, image = local_image)
    base <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.1, height = 0.1, use_cache = FALSE) +
        ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
    nudged <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.1, height = 0.1, nudge_x = 0.2,
                   nudge_y = 0.15, use_cache = FALSE) +
        ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

    base_grob <- coord_image_grobs(ggplot2::ggplotGrob(base))[[1]]
    nudged_grob <- coord_image_grobs(ggplot2::ggplotGrob(nudged))[[1]]
    expect_gt(as.numeric(nudged_grob$x), as.numeric(base_grob$x))
    expect_gt(as.numeric(nudged_grob$y), as.numeric(base_grob$y))
})

test_that("explicit dimensions remain stable under coord_fixed", {
    d <- data.frame(x = 0.5, y = 0.5, image = local_image)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.2, height = 0.1, use_cache = FALSE) +
        ggplot2::coord_fixed()
    rasters <- coord_image_grobs(ggplot2::ggplotGrob(p))

    expect_length(rasters, 1L)
    expect_equal(as.numeric(rasters[[1]]$width), 0.2)
    expect_equal(as.numeric(rasters[[1]]$height), 0.1)
})

test_that("size Inf still creates a full-panel image", {
    d <- data.frame(x = 0.5, y = 0.5, image = local_image)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(size = Inf, use_cache = FALSE)
    rasters <- coord_image_grobs(ggplot2::ggplotGrob(p))

    expect_length(rasters, 1L)
    expect_equal(as.numeric(rasters[[1]]$x), 0.5)
    expect_equal(as.numeric(rasters[[1]]$y), 0.5)
    expect_equal(as.numeric(rasters[[1]]$width), 1)
    expect_equal(as.numeric(rasters[[1]]$height), 1)
})
