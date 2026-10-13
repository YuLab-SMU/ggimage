## ---------------------------------------------------------------------------
## Regression tests for the `hjust` awareness of geom_image_repel().
##
## imageGrob() draws an image with its CENTRE at
##     x + (0.5 - hjust) * width
## (hjust = 0/"left" shifts the centre right by width/2, hjust = 1/"right"
## left by width/2), so `data$x` is an ANCHOR, not the centre, whenever
## hjust != 0.5. The repulsion solver must therefore reason about boxes
## centred on that shifted centre; centring them on the anchor made it
## separate boxes the drawing never uses, and leave alone boxes the drawing
## does overlap.
##
## All assertions are on the geometry that reaches the drawn raster grobs
## (`repel_layout_of()` returns exactly the centre/size imageGrob stored, in
## native panel units), never on pixels.
##
## The fixtures deliberately mix per-row widths: with a uniform width a wrong
## anchor only translates the whole layout rigidly and every pair stays
## separated, which is why this bug was invisible before.
## ---------------------------------------------------------------------------

## per-row width/height go through the aesthetic, not the geom argument, so
## the boxes differ between rows; the panel is the square 0..1 one of
## `repel_square_scales()`, where data units and native units agree
repel_hjust_plot <- function(d, layer) {
    ggplot2::ggplot(d, ggplot2::aes(x, y, image = image,
                                    width = width, height = height)) +
        layer +
        repel_square_scales()
}

repel_hjust_layout <- function(d, hjust, direction = "x", repelled = TRUE) {
    layer <- if (repelled) {
        repel_geom_fun()(hjust = hjust, direction = direction,
                         use_cache = FALSE)
    } else {
        geom_image(hjust = hjust, use_cache = FALSE)
    }
    repel_layout_of(repel_hjust_plot(d, layer))
}

## the maximum overlap of the *drawn* x-intervals [x - width/2, x + width/2]
## over all pairs; > 0 means at least two images are drawn on top of each
## other along x (the fixtures keep every y interval overlapping), <= 0 means
## every pair is separated where it is actually drawn
repel_drawn_x_overlap <- function(layout) {
    lo <- layout$x - layout$width / 2
    hi <- layout$x + layout$width / 2
    best <- -Inf
    for (i in seq_len(nrow(layout) - 1L)) {
        for (j in (i + 1L):nrow(layout)) {
            best <- max(best, min(hi[i], hi[j]) - max(lo[i], lo[j]))
        }
    }
    best
}

## two same-height rows whose ANCHORS are 0.15 apart and whose widths differ
repel_hjust_data <- function() {
    data.frame(
        x = c(0.30, 0.45),
        y = c(0.5, 0.5),
        image = repel_image_file(),
        width = c(0.30, 0.10),
        height = c(0.2, 0.2),
        stringsAsFactors = FALSE
    )
}

test_that("heterogeneous-width images separate along x at every hjust", {
    skip_without_repel_api()

    d <- repel_hjust_data()
    ## premise: drawn by plain geom_image() these two DO overlap for the
    ## off-centre anchors, so there is something for repel to resolve
    stacked <- repel_hjust_layout(d, 0, repelled = FALSE)
    expect_equal(nrow(stacked), 2L)
    expect_gt(repel_drawn_x_overlap(stacked), 0)

    ## the solver must leave NO positive drawn-x-overlap behind, for either
    ## spelling, and the string spelling must agree with the numeric one
    for (spec in list(list(0, "left"), list(1, "right"))) {
        numeric_layout <- repel_hjust_layout(d, spec[[1]])
        string_layout <- repel_hjust_layout(d, spec[[2]])
        expect_equal(numeric_layout$width, c(0.30, 0.10), tolerance = 1e-9)
        expect_equal(string_layout$x, numeric_layout$x, tolerance = 1e-9)
        ## direction = "x" keeps y, and the sizes are untouched
        expect_equal(numeric_layout$y, c(0.5, 0.5), tolerance = 1e-9)
        expect_equal(numeric_layout$height, c(0.2, 0.2), tolerance = 1e-9)
        expect_true(repel_drawn_x_overlap(numeric_layout) <= 0,
                    info = paste0("hjust = ", spec[[1]]))
        gaps <- repel_pair_gaps(numeric_layout)
        expect_gt(gaps$gap[gaps$i == 1L & gaps$j == 2L], 0)
    }
})

test_that("hjust = 0 keeps a drawn-separated layout untouched (fixed point)", {
    skip_without_repel_api()

    ## drawn boxes (hjust = 0 => [x, x + width]): row 1 spans [0.20, 0.30],
    ## row 2 spans [0.35, 0.65] -- 0.05 apart, nothing to resolve. But the
    ## ANCHOR-centred fiction overlaps ([0.15, 0.25] against [0.20, 0.50]),
    ## so a solver that centred the boxes on the anchor could not leave this
    ## input alone.
    d <- data.frame(x = c(0.20, 0.35), y = c(0.5, 0.5),
                    image = repel_image_file(),
                    width = c(0.10, 0.30), height = c(0.2, 0.2),
                    stringsAsFactors = FALSE)
    plain <- repel_hjust_layout(d, 0, repelled = FALSE)
    expect_lte(repel_drawn_x_overlap(plain), 0)

    repelled <- repel_hjust_layout(d, 0)
    expect_equal(repelled$x, plain$x, tolerance = 1e-9)
    expect_equal(repelled$y, plain$y, tolerance = 1e-9)
    expect_equal(repelled$width, plain$width, tolerance = 1e-9)

    ## mirror image for hjust = 1 (drawn box [x - width, x])
    mirror <- data.frame(x = c(0.50, 0.65), y = c(0.5, 0.5),
                         image = repel_image_file(),
                         width = c(0.30, 0.10), height = c(0.2, 0.2),
                         stringsAsFactors = FALSE)
    plain_right <- repel_hjust_layout(mirror, 1, repelled = FALSE)
    expect_lte(repel_drawn_x_overlap(plain_right), 0)
    repelled_right <- repel_hjust_layout(mirror, 1)
    expect_equal(repelled_right$x, plain_right$x, tolerance = 1e-9)
    expect_equal(repelled_right$y, plain_right$y, tolerance = 1e-9)

    ## the same fixture must still be *resolved* at hjust = 0.5, where the
    ## anchor and the drawn centre agree and these boxes do overlap
    expect_gt(repel_drawn_x_overlap(
        repel_hjust_layout(d, 0.5, repelled = FALSE)), 0)
    moved <- repel_hjust_layout(d, 0.5)
    expect_false(isTRUE(all.equal(moved$x, d$x)))
    expect_lte(repel_drawn_x_overlap(moved), 0)
})

test_that("the repulsion writes back an anchor, not the solved centre", {
    skip_without_repel_api()

    ## the row-wise shift must mirror imageGrob() exactly: numeric and string
    ## spellings agree, the sign flips with hjust, and it scales with each
    ## row's own drawn width
    shift <- ggimage:::image_repel_hjust_shift
    w <- c(0.30, 0.10)
    expect_equal(shift(w, 0), w / 2)
    expect_equal(shift(w, "left"), w / 2)
    expect_equal(shift(w, 1), -w / 2)
    expect_equal(shift(w, "right"), -w / 2)
    expect_equal(shift(w, 0.5), c(0, 0))
    expect_equal(shift(w, 0.4), c(0, 0))

    ## end-to-end: the centres imageGrob stores must be exactly what the pure
    ## solver produces when it is fed the shifted centres. If the layer wrote
    ## the solved CENTRE back into data$x instead of subtracting the shift,
    ## imageGrob would shift a second time and this comparison would be off
    ## by width/2 on every row.
    d <- repel_hjust_data()
    for (hjust in c(0, 0.5, 1)) {
        expected <- ggimage:::repel_boxes(
            x = d$x + shift(d$width, hjust), y = d$y,
            width = d$width, height = d$height,
            max.iter = 100, force = 0.1, direction = "x")
        layout <- repel_hjust_layout(d, hjust)
        ## the grob's stored x IS the drawn centre; the solver worked on
        ## centres too, so the two agree directly
        expect_equal(layout$x, expected$x, tolerance = 1e-9)
        expect_equal(layout$y, expected$y, tolerance = 1e-9)
    }
})

test_that("the default hjust = 0.5 path is untouched by hjust awareness", {
    skip_without_repel_api()

    d <- repel_hjust_data()
    implicit <- repel_hjust_layout(d, 0.5)
    explicit <- repel_layout_of(repel_hjust_plot(
        d, repel_geom_fun()(direction = "x", use_cache = FALSE)))
    expect_equal(implicit, explicit)
    expect_lte(repel_drawn_x_overlap(explicit), 0)
})

test_that("three mixed-width rows separate at hjust 0 and 1", {
    skip_without_repel_api()

    d <- data.frame(x = c(0.40, 0.50, 0.45), y = rep(0.5, 3),
                    image = repel_image_file(),
                    width = c(0.25, 0.15, 0.20), height = rep(0.2, 3),
                    stringsAsFactors = FALSE)
    for (hjust in c(0, 0.5, 1, "left", "right")) {
        layout <- repel_hjust_layout(d, hjust)
        expect_equal(nrow(layout), 3L)
        expect_true(repel_drawn_x_overlap(layout) <= 0,
                    info = paste0("hjust = ", hjust))
        gaps <- repel_pair_gaps(layout)
        expect_true(all(gaps$gap > 0), info = paste0("hjust = ", hjust))
    }
})
