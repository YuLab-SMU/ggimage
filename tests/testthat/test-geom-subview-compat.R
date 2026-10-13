## `geom_subview()` used to read its first argument as the subview
## (`geom_subview(subview, x, y, width, height)`).  It reads a mapping there
## now, but a subview is still accepted, because that is the form the published
## examples use and passing one used to fail with a message that never mentioned
## the subview (issue #29).
subview_boxes <- function(layers) {
    data.frame(
        xmin = vapply(layers, function(l) l$geom_params$xmin, numeric(1)),
        xmax = vapply(layers, function(l) l$geom_params$xmax, numeric(1)),
        ymin = vapply(layers, function(l) l$geom_params$ymin, numeric(1)),
        ymax = vapply(layers, function(l) l$geom_params$ymax, numeric(1))
    )
}

subview_data <- function(x, y, size = NULL, subviews) {
    d <- data.frame(x = x, y = y)
    if (!is.null(size)) d$size <- size
    d$subview <- subviews
    d
}

test_that("a leading subview is read as the subview, not as a mapping", {
    sv <- grid::rectGrob()
    subviews <- list(sv, sv, sv)
    x <- c(2, 5, 8)
    y <- c(3, 7, 4)
    size <- c(1, 1.5, 2)

    ## the form the current docs describe, used as the reference
    by_mapping <- ggimage::geom_subview(
        ggplot2::aes(x = x, y = y, subview = subview, width = size, height = size),
        data = subview_data(x, y, size, subviews)
    )
    default_size <- ggimage::geom_subview(
        ggplot2::aes(x = x, y = y, subview = subview),
        data = subview_data(x, y, subviews = subviews)
    )

    ## every positional legacy form must land on the same boxes
    expect_equal(
        subview_boxes(ggimage::geom_subview(subviews, x, y, width = size, height = size)),
        subview_boxes(by_mapping)
    )
    expect_equal(
        subview_boxes(ggimage::geom_subview(subviews, x, y)),
        subview_boxes(default_size)
    )

    ## one subview, positional or named
    expect_equal(
        subview_boxes(ggimage::geom_subview(sv, 6, 5)),
        subview_boxes(ggimage::geom_subview(subview = sv, x = 6, y = 5))
    )
    expect_equal(
        subview_boxes(ggimage::geom_subview(sv, x = 6, y = 5)),
        subview_boxes(ggimage::geom_subview(subview = sv, x = 6, y = 5))
    )

    ## one layer per subview, and the default half-size of .1 is 0.05
    expect_length(ggimage::geom_subview(subviews, x, y), 3L)
    expect_equal(
        subview_boxes(ggimage::geom_subview(subviews, x, y))$xmin,
        x - 0.05
    )
})

test_that("geom_subview() reports what it is missing", {
    ## an aesthetic taken from `ggplot()` cannot be judged at call time: the
    ## check runs when the layer is added, which is also where these forms are
    ## resolved.  The report names the first missing aesthetic.
    expect_error(ggplot2::ggplot() + ggimage::geom_subview(),
                 "needs an `x` aesthetic")
    expect_error(
        ggplot2::ggplot() + ggimage::geom_subview(
            ggplot2::aes(y = y, subview = subview),
            data = subview_data(1, 1, subviews = list(grid::rectGrob()))
        ),
        "needs an `x` aesthetic"
    )
    expect_error(
        ggplot2::ggplot() + ggimage::geom_subview(
            ggplot2::aes(x = x, subview = subview),
            data = subview_data(1, 1, subviews = list(grid::rectGrob()))
        ),
        "needs a `y` aesthetic"
    )
    ## every aesthetic present but no subview anywhere: `ggplot()` supplies x
    ## and y from its mapping and data, so only the subview is truly missing
    expect_error(
        ggplot2::ggplot(subview_data(1:2, 1:2, subviews = list(grid::rectGrob(), grid::rectGrob())),
                        ggplot2::aes(x, y)) + ggimage::geom_subview(),
        "needs a subview"
    )
    ## a first argument that is neither a mapping nor usable as a subview
    expect_error(
        ggimage::geom_subview(list(x = ~x, y = ~y), data = data.frame(x = 1, y = 1)),
        "does not look like an aes\\(\\) mapping"
    )
})

test_that("a subview inherited from the plot is read when the layer is added", {
    subviews <- list(grid::rectGrob(gp = grid::gpar(fill = "red")),
                     grid::rectGrob(gp = grid::gpar(fill = "blue")),
                     grid::rectGrob(gp = grid::gpar(fill = "green")))
    df <- subview_data(c(2, 5, 8), c(3, 7, 4), subviews = subviews)
    reference <- subview_boxes(ggimage::geom_subview(
        ggplot2::aes(x = x, y = y, subview = subview), data = df))
    ## a plot's $layers are named, and those names reach the data.frame as row
    ## labels; compare the columns, not the attribute
    as_columns <- function(boxes) unname(as.list(boxes))

    ## the plot carries all three aesthetics; the layer brings nothing
    all_inherited <- subview_boxes(
        (ggplot2::ggplot(df, ggplot2::aes(x, y, subview = subview)) +
            ggimage::geom_subview())$layers)
    expect_equal(as_columns(all_inherited), as_columns(reference))

    ## the layer brings the subview, the plot brings the positions
    split_across <- subview_boxes(
        (ggplot2::ggplot(df, ggplot2::aes(x, y)) +
            ggimage::geom_subview(ggplot2::aes(subview = subview)))$layers)
    expect_equal(as_columns(split_across), as_columns(reference))

    ## the layer's own data wins over the plot's
    overridden <- (ggplot2::ggplot(df, ggplot2::aes(x, y, subview = subview)) +
        ggimage::geom_subview(
            ggplot2::aes(subview = subview),
            data = subview_data(1, 1, subviews = list(grid::rectGrob()))))$layers
    expect_length(overridden, 1L)
    expect_equal(subview_boxes(overridden)$xmin, 0.95)
})
