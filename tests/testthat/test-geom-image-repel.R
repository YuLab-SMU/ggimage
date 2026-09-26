## ---------------------------------------------------------------------------
## Test contract for the (not yet implemented) `geom_image_repel()` layer.
##
## Contract summary
##   * Observable output is the native-unit x/y/width/height of the drawn
##     image grobs; every assertion below is on that layout, never on pixels.
##   * Units are the ones `width`/`height` already document (R/geom_image.R):
##     native panel units, so the repulsion boxes are
##     [x - hjust*width, x + (1 - hjust)*width] x [y - height/2, y + height/2].
##   * `geom_image_repel()` is expected to behave like `geom_image()` for every
##     argument it inherits, and to additionally displace images so that their
##     boxes stop overlapping, within `max.iter` iterations.
##
## These tests skip while the API is absent, so the suite is green today and
## the contract becomes enforceable the moment the layer is added.
## ---------------------------------------------------------------------------

test_that("geom_image() leaves overlapping images stacked (the premise)", {
    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    p <- repel_plot(d, geom_image(width = 0.2, height = 0.2, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), 2L)
    expect_equal(layout$x, c(0.5, 0.5))
    expect_equal(layout$y, c(0.5, 0.5))
    expect_equal(repel_overlap_count(layout), 1L)
})

test_that("geom_image_repel is exported and has the agreed signature", {
    skip_without_repel_api()

    expect_true("geom_image_repel" %in% getNamespaceExports("ggimage"))
    args <- names(formals(repel_geom_fun()))
    expect_true(all(repel_required_formals %in% args))
    ## defaults must match geom_image() for the inherited arguments
    repel_defaults <- lapply(repel_required_formals[1:12],
                             function(a) formals(repel_geom_fun())[[a]])
    image_defaults <- lapply(repel_required_formals[1:12],
                             function(a) formals(geom_image)[[a]])
    expect_equal(repel_defaults, image_defaults)
    ## documented defaults for the repulsion arguments
    expect_equal(as.numeric(formals(repel_geom_fun())$max.iter), 100)
    expect_match(deparse(formals(repel_geom_fun())$direction), '"both"', fixed = TRUE)
    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5))
    p <- repel_plot(d, repel_geom_fun()())
    expect_true(inherits(p, "ggplot"))
    expect_true(inherits(p$layers[[1]], "LayerInstance"))
})

test_that("overlapping images are pushed apart and no longer overlap", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    p <- repel_plot(d, repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), 2L)
    expect_false(isTRUE(all.equal(layout$x, d$x)) &&
                 isTRUE(all.equal(layout$y, d$y)))
    expect_gt(repel_pair_gaps(layout)$gap[1], 0)
    ## a symmetric two-image conflict resolves around the shared centre
    expect_equal(mean(layout$x), 0.5, tolerance = 1e-6)
    expect_equal(mean(layout$y), 0.5, tolerance = 1e-6)
})

test_that("a cluster of identical images separates every pair", {
    skip_without_repel_api()

    n <- 4L
    d <- repel_data(x = rep(0.5, n), y = rep(0.5, n), width = 0.15, height = 0.15)
    p <- repel_plot(d, repel_geom_fun()(width = 0.15, height = 0.15,
                                        max.iter = 200, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), n)
    expect_equal(repel_overlap_count(layout), 0L)
    expect_equal(mean(layout$x), 0.5, tolerance = 1e-6)
    expect_equal(mean(layout$y), 0.5, tolerance = 1e-6)
})

test_that("a dense cluster improves monotonically with more iterations", {
    skip_without_repel_api()

    d <- repel_data(x = rep(0.5, 12), y = rep(0.5, 12), width = 0.1, height = 0.1)
    layout_for <- function(max.iter) {
        repel_layout_of(repel_plot(
            d, repel_geom_fun()(width = 0.1, height = 0.1,
                                max.iter = max.iter, use_cache = FALSE)))
    }
    one <- layout_for(1)
    ten <- layout_for(10)
    hundred <- layout_for(100)

    min_gap <- function(l) min(repel_pair_gaps(l)$gap)
    expect_lte(repel_overlap_count(one), 66L)
    expect_lte(repel_overlap_count(ten), repel_overlap_count(one))
    expect_lte(repel_overlap_count(hundred), repel_overlap_count(ten))
    expect_gte(min_gap(hundred), -1e-6)
})

test_that("non-overlapping images are a fixed point of the layout", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.1, 0.5, 0.9), y = c(0.1, 0.5, 0.9),
                    width = 0.1, height = 0.1)
    p <- repel_plot(d, repel_geom_fun()(width = 0.1, height = 0.1, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(layout$x, d$x)
    expect_equal(layout$y, d$y)
})

test_that("max.iter = 0 keeps the original coordinates", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5, 0.5), y = c(0.4, 0.4, 0.4),
                    width = 0.2, height = 0.2)
    p <- repel_plot(d, repel_geom_fun()(width = 0.2, height = 0.2,
                                        max.iter = 0, use_cache = FALSE))
    layout <- repel_layout_of(p)

    expect_equal(layout$x, d$x)
    expect_equal(layout$y, d$y)
    ## the boxes are untouched as well
    expect_equal(layout$width, d$width)
    expect_equal(layout$height, d$height)
    expect_equal(repel_overlap_count(layout), 3L)
})

test_that("direction constrains which coordinate moves", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.3, height = 0.3)

    along_x <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.3, height = 0.3,
                            direction = "x", use_cache = FALSE)))
    expect_equal(along_x$y, c(0.5, 0.5))
    expect_gt(abs(diff(along_x$x)), 0)
    ## the whole displacement has to fit inside the box width
    expect_gte(abs(diff(along_x$x)), 0.3)

    along_y <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.3, height = 0.3,
                            direction = "y", use_cache = FALSE)))
    expect_equal(along_y$x, c(0.5, 0.5))
    expect_gte(abs(diff(along_y$y)), 0.3)

    both <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.3, height = 0.3, use_cache = FALSE)))
    expect_false(isTRUE(all.equal(both$x, c(0.5, 0.5))) &&
                 isTRUE(all.equal(both$y, c(0.5, 0.5))))

    ## an unsupported direction is rejected, not silently ignored
    expect_error(
        ggplot2::ggplotGrob(repel_plot(
            d, repel_geom_fun()(width = 0.3, height = 0.3,
                                direction = "diagonal", use_cache = FALSE))),
        "direction"
    )
})

test_that("direction = x cannot fix a conflict that only differs in y", {
    skip_without_repel_api()

    ## stacked vertically, so horizontal-only repulsion is powerless: the
    ## contract is "no worse than the input", not a solved layout
    d <- repel_data(x = rep(0.5, 3), y = c(0.55, 0.5, 0.45), width = 0.1, height = 0.2)
    layout <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.1, height = 0.2,
                            direction = "x", max.iter = 50, use_cache = FALSE)))
    stacked <- repel_layout_of(repel_plot(
        d, geom_image(width = 0.1, height = 0.2, use_cache = FALSE)))

    expect_equal(layout$y, d$y)
    expect_lte(repel_overlap_count(layout), repel_overlap_count(stacked))
})

test_that("force scales how far images are pushed per iteration", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    gap_for <- function(force) {
        layout <- repel_layout_of(repel_plot(
            d, repel_geom_fun()(width = 0.2, height = 0.2, force = force,
                                max.iter = 1, use_cache = FALSE)))
        repel_pair_gaps(layout)$gap[1]
    }
    weak <- gap_for(0.1)
    strong <- gap_for(4)

    expect_gt(strong, weak)
    ## force must be a single finite, non-negative number
    expect_error(ggplot2::ggplotGrob(repel_plot(
        d, repel_geom_fun()(force = -1, use_cache = FALSE))), "force")
    expect_error(ggplot2::ggplotGrob(repel_plot(
        d, repel_geom_fun()(force = c(1, 2), use_cache = FALSE))), "force")
})

test_that("box.padding opens a gap between neighbouring images", {
    skip_without_repel_api()

    ## two boxes that just touch: with padding 0 they may stay as they are,
    ## with padding they must move apart
    d <- repel_data(x = c(0.4, 0.6), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    gap_for <- function(box.padding) {
        layout <- repel_layout_of(repel_plot(
            d, repel_geom_fun()(width = 0.2, height = 0.2, box.padding = box.padding,
                                max.iter = 50, use_cache = FALSE)))
        repel_pair_gaps(layout)$gap[1]
    }
    expect_gte(gap_for(0), 0)
    expect_gt(gap_for(0.5), gap_for(0))
    expect_gt(gap_for(0.2) + 1e-6, gap_for(0))
    expect_error(ggplot2::ggplotGrob(repel_plot(
        d, repel_geom_fun()(box.padding = -0.5, use_cache = FALSE))), "box.padding")
})

test_that("the repulsion box is the drawn box for one- and two-sided sizes", {
    skip_without_repel_api()

    ratio <- ggimage:::getAR2(magick::image_read(repel_image_file()))

    ## both given: exact box
    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.1)
    both <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.1, use_cache = FALSE)))
    expect_equal(both$width, c(0.2, 0.2))
    expect_equal(both$height, c(0.1, 0.1))

    ## only width given: height keeps the image ratio, and that derived height
    ## is what the layout has to separate
    d_one <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5))
    width_only <- repel_layout_of(repel_plot(
        d_one, repel_geom_fun()(width = 0.2, use_cache = FALSE)))
    expect_equal(width_only$width, c(0.2, 0.2))
    expect_equal(width_only$height, rep(0.2 / ratio, 2))
    ## 0.2/ratio < 0.2, so resolving this conflict needs a horizontal split
    expect_gte(abs(diff(width_only$x)), 0.2)
    expect_gte(abs(diff(width_only$y)), 0.2 / ratio - 1e-6)

    ## only height given: width keeps the image ratio
    height_only <- repel_layout_of(repel_plot(
        d_one, repel_geom_fun()(height = 0.1, use_cache = FALSE)))
    expect_equal(height_only$width, rep(0.1 * ratio, 2))
    expect_equal(height_only$height, c(0.1, 0.1))
    expect_gte(abs(diff(height_only$y)), 0.1)
})

test_that("a taller image pushes its neighbour further along y", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5))
    short <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.1, height = 0.1, use_cache = FALSE)))
    tall <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.1, height = 0.4, use_cache = FALSE)))

    ## displacement is driven by the box that has to be cleared
    expect_gt(max(tall$y) - min(tall$y), max(short$y) - min(short$y))
    expect_equal(repel_overlap_count(short), 0L)
    expect_equal(repel_overlap_count(tall), 0L)
})

test_that("per-row width and height participate in the layout", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5),
                    width = c(0.1, 0.4), height = c(0.4, 0.1))
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image,
                                         width = width, height = height)) +
        repel_geom_fun()(use_cache = FALSE) +
        repel_square_scales()
    layout <- repel_layout_of(p)

    expect_equal(layout$width, c(0.1, 0.4))
    expect_equal(layout$height, c(0.4, 0.1))
    expect_equal(repel_overlap_count(layout), 0L)
    ## the wide image travels further in x, the tall one further in y
    expect_gte(abs(layout$x[2] - d$x[2]), abs(layout$x[1] - d$x[1]))
})

test_that("NA width or height falls back to size/by and still repels", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5))
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image, width = width)) +
        repel_geom_fun()(width = c(0.2, NA_real_), size = 0.05,
                         use_cache = FALSE) +
        repel_square_scales()
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), 2L)
    expect_true(all(is.finite(layout$x)) && all(is.finite(layout$y)))
    expect_equal(layout$width[1], 0.2)
    ## the row without an explicit width keeps the size/by box
    expect_false(isTRUE(all.equal(layout$width[2], 0.2)))
    expect_gt(repel_pair_gaps(layout)$gap[1], 0)
})

test_that("coord_fixed does not reinterpret the repelled layout", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.1)
    without <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.1, use_cache = FALSE)))
    fixed_p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        repel_geom_fun()(width = 0.2, height = 0.1, use_cache = FALSE) +
        repel_square_scales() +
        ggplot2::coord_fixed()
    with <- repel_layout_of(fixed_p)

    expect_equal(with, without)
    expect_equal(repel_overlap_count(with), 0L)
})

test_that("nudge shifts the resolved layout without changing the separations", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    plain <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE)))
    nudged <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.2,
                            nudge_x = 0.05, nudge_y = 0.04, use_cache = FALSE)))

    ## nudging happens before the layout, so it is a pure translation
    expect_equal(nudged$x - 0.05, plain$x, tolerance = 1e-6)
    expect_equal(nudged$y - 0.04, plain$y, tolerance = 1e-6)
    expect_equal(repel_pair_gaps(nudged), repel_pair_gaps(plain))
})

test_that("hjust anchors the repulsion box the same way the image is drawn", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    centered <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE)))
    left <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.2, hjust = 0, use_cache = FALSE)))
    right <- repel_layout_of(repel_plot(
        d, repel_geom_fun()(width = 0.2, height = 0.2, hjust = 1, use_cache = FALSE)))

    ## hjust reaches the drawn images: imageGrob stores the *centre* of the
    ## drawn box, so an anchor change translates every centre by +-width/2
    shift_left <- left$x - centered$x
    shift_right <- right$x - centered$x
    expect_equal(shift_left, -shift_right, tolerance = 1e-6)
    expect_gt(shift_left[1], 0)
    ## the pairwise slack does not depend on the anchor, and the resolved
    ## boxes still do not overlap once measured around their own centre
    expect_equal(repel_pair_gaps(left)$gap, repel_pair_gaps(centered)$gap,
                 tolerance = 1e-6)
    expect_equal(repel_pair_gaps(right)$gap, repel_pair_gaps(centered)$gap,
                 tolerance = 1e-6)
    expect_equal(repel_overlap_count(left), 0L)
    expect_equal(repel_overlap_count(right), 0L)
})

test_that("single row and empty input are handled", {
    skip_without_repel_api()

    one <- repel_data(x = 0.5, y = 0.5, width = 0.2, height = 0.2)
    layout <- repel_layout_of(repel_plot(
        one, repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE)))
    expect_equal(layout$x, 0.5)
    expect_equal(layout$y, 0.5)
    expect_equal(layout$width, 0.2)
    expect_equal(layout$height, 0.2)

    empty <- repel_data(numeric(), numeric())
    empty_p <- ggplot2::ggplot(empty, ggplot2::aes(x, y, image = image)) +
        repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE) +
        repel_square_scales()
    expect_equal(nrow(repel_layout_of(empty_p)), 0L)
    expect_equal(nrow(ggplot2::ggplot_build(empty_p)$data[[1]]), 0L)
})

test_that("missing coordinates are dropped the way geom_image drops them", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, NA_real_, 0.5), y = c(0.5, 0.5, NA_real_),
                    width = 0.2, height = 0.2)
    repel_p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE) +
        repel_square_scales()
    image_p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        geom_image(width = 0.2, height = 0.2, use_cache = FALSE) +
        repel_square_scales()

    ## na.rm = FALSE warns about the dropped rows, exactly like geom_image()
    expect_warning(repel_layout <- repel_layout_of(repel_p), "missing")
    expect_warning(image_layout <- repel_layout_of(image_p), "missing")
    expect_equal(nrow(repel_layout), 1L)
    expect_equal(nrow(image_layout), 1L)
    ## the surviving row has no neighbour, so it must not move
    expect_equal(repel_layout$x, image_layout$x)
    expect_equal(repel_layout$y, image_layout$y)

    quiet <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        repel_geom_fun()(width = 0.2, height = 0.2, na.rm = TRUE, use_cache = FALSE) +
        repel_square_scales()
    expect_silent(repel_layout_of(quiet))
})

test_that("faceted panels repel independently", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5, 0.5, 0.5), y = c(0.5, 0.5, 0.5, 0.5),
                    width = 0.2, height = 0.2)
    d$panel <- rep(c("a", "b"), each = 2)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
        repel_geom_fun()(width = 0.2, height = 0.2, use_cache = FALSE) +
        repel_square_scales() +
        ggplot2::facet_wrap(~panel)
    layout <- repel_layout_of(p)

    expect_equal(nrow(layout), 4L)
    ## each panel resolves on its own, exactly like the two-image case
    first <- layout[1:2, ]
    second <- layout[3:4, ]
    expect_gt(repel_pair_gaps(first)$gap[1], 0)
    expect_gt(repel_pair_gaps(second)$gap[1], 0)
    expect_equal(unname(as.matrix(first)), unname(as.matrix(second)))
})

test_that("size = Inf does not crash the repulsion layout", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.4, 0.6), y = c(0.4, 0.6))
    p <- repel_plot(d, repel_geom_fun()(size = Inf, max.iter = 20, use_cache = FALSE))

    expect_silent(layout <- repel_layout_of(p))
    expect_equal(nrow(layout), 2L)
    expect_true(all(is.finite(layout$x)))
    expect_true(all(is.finite(layout$y)))
    expect_no_error(ggplot2::ggplot_build(p))
})
test_that("the layout is deterministic: same input, same output", {
    skip_without_repel_api()

    d <- repel_data(x = rep(0.5, 6), y = rep(0.5, 6), width = 0.15, height = 0.15)
    build <- function() {
        repel_layout_of(repel_plot(
            d, repel_geom_fun()(width = 0.15, height = 0.15,
                                max.iter = 50, use_cache = FALSE)))
    }
    first <- build()
    expect_equal(build(), first)
    expect_equal(
        withr::with_seed(1, build()),
        withr::with_seed(20260926, build())
    )
    ## a shuffled input resolves to the same set of boxes
    shuffled <- d[sample.int(nrow(d)), ]
    shuffled_layout <- repel_layout_of(repel_plot(
        shuffled, repel_geom_fun()(width = 0.15, height = 0.15,
                                   max.iter = 50, use_cache = FALSE)))
    expect_equal(sort(shuffled_layout$x), sort(first$x), tolerance = 1e-6)
    expect_equal(sort(shuffled_layout$y), sort(first$y), tolerance = 1e-6)
})


test_that("large dense layouts stay finite and deterministic", {
    skip_without_repel_api()

    solve <- function(n, direction = "both") {
        ggimage:::repel_boxes(
            x = rep(0.5, n), y = rep(0.5, n),
            width = rep(0.1, n), height = rep(0.1, n),
            max.iter = 100, force = 0.1, direction = direction
        )
    }

    ## These sizes exercise the cleanup cap without asserting wall-clock time.
    ## The solver must return a reproducible finite layout rather than scaling
    ## cleanup sweeps linearly with n.
    for (n in c(50L, 100L)) {
        first <- solve(n)
        second <- solve(n)
        expect_equal(length(first$x), n)
        expect_equal(length(first$y), n)
        expect_true(all(is.finite(first$x)) && all(is.finite(first$y)))
        expect_equal(first, second)
        expect_gt(max(first$x) - min(first$x), 0)
        expect_gt(max(first$y) - min(first$y), 0)
    }

    ## Restricting movement to one axis remains an invariant at this scale.
    x_only <- solve(50L, direction = "x")
    y_only <- solve(50L, direction = "y")
    expect_equal(x_only$y, rep(0.5, 50L))
    expect_equal(y_only$x, rep(0.5, 50L))
    expect_true(all(is.finite(x_only$x)) && all(is.finite(y_only$y)))
})
