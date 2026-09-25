## ---------------------------------------------------------------------------
## Shared helpers for the geom_image_repel() test contract.
##
## The contract deliberately only looks at observable geometry: the
## native-panel-unit x/y/width/height that reach the drawn grobs. Nothing here
## renders to a device, downloads an image, or compares raster output, so the
## whole suite runs offline.
##
## `repel_api_available()` gates the tests for the layer that does not exist
## yet; they skip (rather than fail) until geom_image_repel() lands, and turn
## into real assertions without touching this file.
## ---------------------------------------------------------------------------

repel_image_file <- function() {
    system.file("extdata/Rlogo.png", package = "ggimage")
}

## the geom function, resolved from the namespace so the tests do not depend on
## whether it is exported yet (exported-ness is asserted separately)
repel_geom_fun <- function() {
    if (!repel_api_available()) {
        stop("geom_image_repel() is not available", call. = FALSE)
    }
    get0("geom_image_repel", envir = asNamespace("ggimage"), inherits = FALSE)
}

## the proposed pure layout helper, see test-repel-layout.R
repel_layout_fun <- function() {
    if (!repel_layout_available()) {
        stop("repel_image_positions() is not available", call. = FALSE)
    }
    get0("repel_image_positions", envir = asNamespace("ggimage"), inherits = FALSE)
}

repel_api_available <- function() {
    exists("geom_image_repel", envir = asNamespace("ggimage"), inherits = FALSE)
}

repel_layout_available <- function() {
    exists("repel_image_positions", envir = asNamespace("ggimage"), inherits = FALSE)
}

skip_without_repel_api <- function() {
    if (!repel_api_available()) {
        skip("geom_image_repel() is not implemented yet")
    }
    invisible(TRUE)
}

skip_without_repel_layout <- function() {
    if (!repel_layout_available()) {
        skip("repel_image_positions() is not implemented yet")
    }
    invisible(TRUE)
}

## every layer must expose these; `...` is allowed on top
repel_required_formals <- c(
    "mapping", "data", "stat", "position", "inherit.aes", "na.rm", "by",
    "nudge_x", "nudge_y", "use_cache", "width", "height",
    "max.iter", "direction", "force", "box.padding"
)

## a data frame with one image per row, ready for aes(x, y, image = image)
repel_data <- function(x, y, width = NULL, height = NULL,
                       image = repel_image_file()) {
    n <- length(x)
    out <- data.frame(x = x, y = y, image = rep_len(image, n), stringsAsFactors = FALSE)
    if (!is.null(width)) out$width <- rep_len(width, n)
    if (!is.null(height)) out$height <- rep_len(height, n)
    out
}

## a square 0..1 panel with no expansion, so data units and native units agree
repel_square_scales <- function() {
    list(
        ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)),
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
    )
}

## a plot whose data coordinates are also its native panel coordinates
repel_plot <- function(d, geom) {
    ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) + geom +
        repel_square_scales()
}

repel_grobs <- function(x, cls = "rastergrob") {
    out <- list()
    if (inherits(x, cls)) out[[length(out) + 1L]] <- x
    children <- list(x$children, x$grobs)
    for (items in children) {
        if (!is.null(items)) {
            for (child in items) out <- c(out, repel_grobs(child, cls))
        }
    }
    out
}

repel_unit_value <- function(grob, name) {
    value <- grob[[name]]
    if (is.null(value)) return(NA_real_)
    as.numeric(value)
}

## native-unit layout of the drawn images, in data-row order.
## zeroGrob children (invalid/NA images) are not returned.
repel_layout_of <- function(plot) {
    grobs <- repel_grobs(ggplot2::ggplotGrob(plot))
    if (length(grobs) == 0L) {
        return(data.frame(x = numeric(), y = numeric(),
                          width = numeric(), height = numeric()))
    }
    data.frame(
        x = vapply(grobs, repel_unit_value, numeric(1), name = "x"),
        y = vapply(grobs, repel_unit_value, numeric(1), name = "y"),
        width = vapply(grobs, repel_unit_value, numeric(1), name = "width"),
        height = vapply(grobs, repel_unit_value, numeric(1), name = "height")
    )
}

## the axis-aligned box of a row. `x` is the anchor: `repel_layout_of()`
## returns grob coordinates, which imageGrob already stores at the centre of
## the drawn image, so pass hjust = 0.5 for those. `hjust` only matters when
## the box is derived from an untransformed data coordinate.
repel_box <- function(row, hjust = 0.5) {
    right <- row$x + (1 - hjust) * row$width
    list(x0 = right - row$width, x1 = right,
         y0 = row$y - row$height / 2, y1 = row$y + row$height / 2)
}

## > 0 when the two boxes are separated, and the amount of slack along the
## better-separated axis; <= 0 when they overlap.
repel_pair_gap <- function(a, b, hjust = 0.5) {
    box_a <- repel_box(a, hjust)
    box_b <- repel_box(b, hjust)
    gap_x <- max(box_a$x0 - box_b$x1, box_b$x0 - box_a$x1)
    gap_y <- max(box_a$y0 - box_b$y1, box_b$y0 - box_a$y1)
    max(gap_x, gap_y)
}

## all pairwise gaps, as a data frame of row indices i < j
repel_pair_gaps <- function(layout, hjust = 0.5) {
    n <- nrow(layout)
    if (n < 2L) return(data.frame(i = integer(), j = integer(), gap = numeric()))
    pairs <- t(utils::combn(n, 2))
    gaps <- apply(pairs, 1, function(id) {
        repel_pair_gap(layout[id[1], ], layout[id[2], ], hjust = hjust)
    })
    data.frame(i = pairs[, 1], j = pairs[, 2], gap = as.numeric(gaps))
}

## number of overlapping pairs; the objective a repulsion layout has to reduce
repel_overlap_count <- function(layout, hjust = 0.5, tolerance = 1e-6) {
    gaps <- repel_pair_gaps(layout, hjust = hjust)
    sum(gaps$gap <= tolerance)
}
