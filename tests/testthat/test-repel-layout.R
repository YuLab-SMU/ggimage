## ---------------------------------------------------------------------------
## Contract for the pure layout helper proposed as the minimal public API for
## geom_image_repel():
##
##   repel_image_positions(x, y, width, height,
##                         max.iter = 10,
##                         direction = c("both", "x", "y"),
##                         force = 1,
##                         box.padding = 0.5,
##                         hjust = 0.5)
##
##   -> data.frame(x, y, width, height), one row per input row, all values in
##      native panel units.
##
## Why a helper is needed: every observable above has to be produced through a
## ggplot build (ggplotGrob -> coord$transform -> draw_panel), which makes it
## awkward to assert the properties that actually define the algorithm — the
## fixed point, idempotence, the invariants on input validation, and the fact
## that the layout resizes nothing. Those are cheap and unambiguous on a pure
## function, and the geom-level tests in test-geom-image-repel.R stay the
## end-to-end guard. Without exporting it, the same assertions have to be
## re-derived through the plot pipeline (or through ggimage:::, which is not a
## stable target for a documented API).
##
## These tests skip until the helper exists.
## ---------------------------------------------------------------------------

test_that("the layout helper is exported, pure and shape-preserving", {
    skip_without_repel_layout()

    expect_true("repel_image_positions" %in% getNamespaceExports("ggimage"))
    args <- names(formals(repel_layout_fun()))
    expect_true(all(c("x", "y", "width", "height", "max.iter", "direction",
                      "force", "box.padding") %in% args))

    x <- c(0.5, 0.5)
    y <- c(0.5, 0.5)
    w <- c(0.2, 0.2)
    h <- c(0.2, 0.2)
    out <- repel_layout_fun()(x = x, y = y, width = w, height = h)

    expect_s3_class(out, "data.frame")
    expect_named(out, c("x", "y", "width", "height"), ignore.order = TRUE)
    expect_equal(nrow(out), 2L)
    ## the inputs are never modified in place
    expect_equal(x, c(0.5, 0.5))
    expect_equal(y, c(0.5, 0.5))
    expect_equal(w, c(0.2, 0.2))
    expect_equal(h, c(0.2, 0.2))
})

test_that("the layout moves boxes, it never resizes them", {
    skip_without_repel_layout()

    widths <- c(0.1, 0.3, 0.05)
    heights <- c(0.2, 0.1, 0.4)
    out <- repel_layout_fun()(x = c(0.5, 0.5, 0.5), y = c(0.5, 0.5, 0.5),
                              width = widths, height = heights,
                              max.iter = 100, box.padding = 0.3)

    expect_equal(out$width, widths)
    expect_equal(out$height, heights)
    expect_equal(repel_overlap_count(out), 0L)
})

test_that("scalar width and height are recycled over rows", {
    skip_without_repel_layout()

    n <- 5L
    out <- repel_layout_fun()(x = rep(0.5, n), y = rep(0.5, n),
                              width = 0.1, height = 0.1, max.iter = 100)
    expect_equal(nrow(out), n)
    expect_equal(rep_len(out$width, n), rep(0.1, n))
    expect_equal(rep_len(out$height, n), rep(0.1, n))

    ## a non-recyclable length is rejected instead of silently truncated
    expect_error(repel_layout_fun()(x = 1:2, y = 1:2,
                                    width = rep(0.1, 3), height = rep(0.1, 3)))
})

test_that("degenerate input is handled", {
    skip_without_repel_layout()

    empty <- repel_layout_fun()(x = numeric(), y = numeric(),
                                width = numeric(), height = numeric())
    expect_equal(nrow(empty), 0L)
    expect_named(empty, c("x", "y", "width", "height"), ignore.order = TRUE)

    single <- repel_layout_fun()(x = 0.5, y = 0.5, width = 0.2, height = 0.2)
    expect_equal(single$x, 0.5)
    expect_equal(single$y, 0.5)

    ## a single point on top of nothing is a fixed point for any max.iter
    again <- repel_layout_fun()(x = 0.5, y = 0.5, width = 0.2, height = 0.2,
                                max.iter = 500, force = 10)
    expect_equal(again, single)

    ## zero-area boxes cannot conflict, so nothing moves
    flat <- repel_layout_fun()(x = c(0.5, 0.5), y = c(0.5, 0.5),
                               width = 0, height = 0)
    expect_equal(flat$x, c(0.5, 0.5))
    expect_equal(flat$y, c(0.5, 0.5))

    ## non-finite boxes are reported, not turned into NaN coordinates
    expect_error(repel_layout_fun()(x = c(0.5, 0.5), y = c(0.5, 0.5),
                                    width = c(Inf, 0.2), height = c(0.2, 0.2)))
})

test_that("missing coordinates pass through and do not disturb their neighbours", {
    skip_without_repel_layout()

    with_na <- repel_layout_fun()(x = c(0.5, NA_real_, 0.5),
                                  y = c(0.5, 0.5, NA_real_),
                                  width = c(0.2, 0.2, 0.2),
                                  height = c(0.2, 0.2, 0.2),
                                  max.iter = 100)
    without <- repel_layout_fun()(x = c(0.5, 0.5), y = c(0.5, 0.5),
                                  width = c(0.2, 0.2), height = c(0.2, 0.2),
                                  max.iter = 100)

    expect_true(is.na(with_na$x[2]))
    expect_true(is.na(with_na$y[3]))
    ## the two usable rows resolve exactly as if the broken rows were absent
    expect_equal(with_na$x[c(1, 3)], without$x, tolerance = 1e-6)
    expect_equal(with_na$y[c(1, 3)], without$y, tolerance = 1e-6)
})

test_that("max.iter = 0 is the identity and invalid controls are rejected", {
    skip_without_repel_layout()

    x <- c(0.5, 0.5, 0.5)
    y <- c(0.5, 0.5, 0.5)
    out <- repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2, max.iter = 0)
    expect_equal(out$x, x)
    expect_equal(out$y, y)

    expect_error(repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2,
                                    max.iter = -1))
    expect_error(repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2,
                                    max.iter = 1.5))
    expect_error(repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2,
                                    direction = "sideways"))
    expect_error(repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2,
                                    force = -1))
    expect_error(repel_layout_fun()(x = x, y = y, width = 0.2, height = 0.2,
                                    box.padding = -1))
})

test_that("a solved layout is a fixed point (idempotence)", {
    skip_without_repel_layout()

    once <- repel_layout_fun()(x = rep(0.5, 3), y = rep(0.5, 3),
                               width = 0.2, height = 0.2, max.iter = 200)
    expect_equal(repel_overlap_count(once), 0L)

    twice <- repel_layout_fun()(x = once$x, y = once$y,
                                width = once$width, height = once$height,
                                max.iter = 200)
    expect_equal(twice, once)
})

test_that("a symmetric conflict resolves symmetrically", {
    skip_without_repel_layout()

    out <- repel_layout_fun()(x = c(0.5, 0.5), y = c(0.5, 0.5),
                              width = 0.2, height = 0.2, max.iter = 100)
    dx <- out$x - 0.5
    dy <- out$y - 0.5

    ## equal and opposite displacement around the shared centre
    expect_equal(dx[1], -dx[2], tolerance = 1e-6)
    expect_equal(dy[1], -dy[2], tolerance = 1e-6)
    expect_gt(dx[1], 0)
    expect_equal(repel_overlap_count(out), 0L)
})

test_that("direction constrains the layout", {
    skip_without_repel_layout()

    args <- list(x = rep(0.5, 3), y = rep(0.5, 3), width = 0.2, height = 0.2,
                 max.iter = 100)
    x_only <- do.call(repel_layout_fun(), c(args, list(direction = "x")))
    y_only <- do.call(repel_layout_fun(), c(args, list(direction = "y")))

    expect_equal(x_only$y, args$y)
    expect_equal(length(unique(round(x_only$x, 6))), 3L)
    expect_gte(min(abs(diff(x_only$x))), 0.2 - 1e-6)
    expect_equal(y_only$x, args$x)
    expect_equal(length(unique(round(y_only$y, 6))), 3L)
    expect_gte(min(abs(diff(y_only$y))), 0.2 - 1e-6)
})

test_that("force and box.padding push monotonically harder", {
    skip_without_repel_layout()

    args <- list(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    gap <- function(...) {
        out <- do.call(repel_layout_fun(), c(args, list(...)))
        repel_pair_gaps(out)$gap[1]
    }

    ## a single iteration cannot fully solve the conflict, so it shows the
    ## strength of the control being varied
    expect_gt(gap(force = 8, max.iter = 1), gap(force = 0.2, max.iter = 1))
    expect_gt(gap(box.padding = 0.8, max.iter = 1),
              gap(box.padding = 0, max.iter = 1))
    ## at convergence padding still widens the clearance
    expect_gt(gap(box.padding = 0.8), gap(box.padding = 0))
})

test_that("the layout is deterministic and free of random state", {
    skip_without_repel_layout()

    args <- list(x = rep(0.5, 8), y = rep(0.5, 8), width = 0.1, height = 0.1,
                 max.iter = 100)
    first <- do.call(repel_layout_fun(), args)
    expect_equal(do.call(repel_layout_fun(), args), first)
    expect_equal(withr::with_seed(7, do.call(repel_layout_fun(), args)), first)
    expect_equal(withr::with_seed(4242, do.call(repel_layout_fun(), args)), first)
    ## row order must not decide who wins a conflict
    crowded <- list(x = seq(0.35, 0.65, length.out = 6), y = rep(0.5, 6),
                    width = rep(0.2, 6), height = rep(0.2, 6), max.iter = 200)
    forward <- do.call(repel_layout_fun(), crowded)
    backward <- do.call(repel_layout_fun(),
                        c(lapply(crowded[1:4], rev), crowded[5]))
    expect_equal(sort(backward$x), sort(forward$x), tolerance = 1e-6)
    expect_equal(sort(backward$y), sort(forward$y), tolerance = 1e-6)
})
