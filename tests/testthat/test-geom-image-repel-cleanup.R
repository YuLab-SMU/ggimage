## ---------------------------------------------------------------------------
## The cleanup cap of repel_boxes() is a hard bound (repel_cleanup_limit
## sweeps), so a dense enough layout can run out of sweeps while pairs still
## overlap -- and is then DRAWN with visible residual overlap. The contract:
## that situation emits exactly ONE warning() per repel_boxes() call naming
## the residual depth and the knobs that help; the ordinary converged path,
## and every layout that fits inside the cap, stay silent.
##
## `cleanup.limit` is an internal argument that exists precisely so these
## tests can reach the cap deterministically instead of building a grid dense
## enough to exhaust the default 256.
## ---------------------------------------------------------------------------

repel_collect_warnings <- function(expr) {
    msgs <- character()
    value <- withCallingHandlers(
        expr,
        warning = function(w) {
            msgs <<- c(msgs, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(value = value, warnings = msgs)
}

## a cluster that cannot be resolved to zero overlap in a handful of sweeps
repel_dense_cluster <- function(n = 8, size = 0.1) {
    ggimage:::repel_boxes(
        x = rep(0.5, n), y = rep(0.5, n),
        width = rep(size, n), height = rep(size, n),
        max.iter = 100, force = 0.1, direction = "both"
    )
}

test_that("a tiny cleanup cap on an overlapping layout warns once", {
    captured <- repel_collect_warnings(
        ggimage:::repel_boxes(
            x = rep(0.5, 8), y = rep(0.5, 8),
            width = rep(0.1, 8), height = rep(0.1, 8),
            max.iter = 100, force = 0.1, direction = "both",
            cleanup.limit = 1L
        )
    )

    expect_length(captured$warnings, 1L)
    expect_match(captured$warnings[1], "partially resolved", fixed = TRUE)
    expect_match(captured$warnings[1], "panel units of overlap remain",
                 fixed = TRUE)
    ## the residual depth is named, and the knobs are pointed at
    expect_match(captured$warnings[1],
                 "[0-9]\\.[0-9]+ panel units of overlap",
                 perl = TRUE)
    expect_match(captured$warnings[1], "max.iter", fixed = TRUE)
    expect_match(captured$warnings[1], "force", fixed = TRUE)
    expect_match(captured$warnings[1], "box.padding", fixed = TRUE)
    ## the cap is named too
    expect_match(captured$warnings[1], "1 cleanup sweeps", fixed = TRUE)

    ## the returned layout is still a plain list of finite coordinates
    expect_true(is.list(captured$value))
    expect_true(all(is.finite(captured$value$x)))
    expect_true(all(is.finite(captured$value$y)))
})

test_that("the default cap does not warn for the same layout", {
    ## the same eight-image cluster that overflows a 1-sweep cap converges
    ## inside the default budget, so it must stay silent
    captured <- repel_collect_warnings(
        repel_dense_cluster()
    )
    expect_length(captured$warnings, 0L)

    ## and a small three-image layout resolves inside the budget too
    captured_small <- repel_collect_warnings(
        ggimage:::repel_boxes(
            x = c(0.40, 0.50, 0.60), y = c(0.5, 0.5, 0.5),
            width = rep(0.15, 3), height = rep(0.15, 3)
        )
    )
    expect_length(captured_small$warnings, 0L)
})

test_that("an already separated layout never warns", {
    captured <- repel_collect_warnings(
        ggimage:::repel_boxes(
            x = c(0.1, 0.9), y = c(0.1, 0.9),
            width = c(0.1, 0.1), height = c(0.1, 0.1)
        )
    )
    expect_length(captured$warnings, 0L)
    ## untouched, as the solver has nothing to do
    expect_equal(captured$value$x, c(0.1, 0.9))
    expect_equal(captured$value$y, c(0.1, 0.9))

    ## a single row cannot overlap anything, whatever the cap
    captured_one <- repel_collect_warnings(
        ggimage:::repel_boxes(
            x = 0.5, y = 0.5, width = 0.5, height = 0.5, cleanup.limit = 1L
        )
    )
    expect_length(captured_one$warnings, 0L)

    ## max.iter = 0 is documented to keep the original coordinates and never
    ## reaches the cleanup loop
    captured_zero <- repel_collect_warnings(
        ggimage:::repel_boxes(
            x = rep(0.5, 8), y = rep(0.5, 8),
            width = rep(0.1, 8), height = rep(0.1, 8),
            max.iter = 0, cleanup.limit = 1L
        )
    )
    expect_length(captured_zero$warnings, 0L)
    expect_equal(captured_zero$value$x, rep(0.5, 8))
})

test_that("the warning fires once per call, not once per overlapping pair", {
    ## 40 images on one point are 780 overlapping pairs; the cap is exhausted
    ## on the first sweep, so this counts the conditions, not the matches
    for (n in c(20L, 40L, 80L)) {
        captured <- repel_collect_warnings(
            ggimage:::repel_boxes(
                x = rep(0.5, n), y = rep(0.5, n),
                width = rep(0.05, n), height = rep(0.05, n),
                max.iter = 100, force = 0.1, direction = "both",
                cleanup.limit = 2L
            )
        )
        expect_length(captured$warnings, 1L)
    }

    ## one warning per repel_boxes() call also means: a second call warns
    ## again (the signal is per layout, not a session-once latch)
    captured <- repel_collect_warnings({
        ggimage:::repel_boxes(
            x = rep(0.5, 8), y = rep(0.5, 8),
            width = rep(0.1, 8), height = rep(0.1, 8), cleanup.limit = 1L)
        ggimage:::repel_boxes(
            x = rep(0.5, 8), y = rep(0.5, 8),
            width = rep(0.1, 8), height = rep(0.1, 8), cleanup.limit = 1L)
    })
    expect_length(captured$warnings, 2L)
})

test_that("the cap is a named constant and the default budget is 256", {
    expect_identical(ggimage:::repel_cleanup_limit, 256L)
    formals_defaults <- deparse(formals(ggimage:::repel_boxes)$cleanup.limit)
    expect_equal(formals_defaults, "repel_cleanup_limit")
})

test_that("the geom layer surfaces the cleanup warning while drawing", {
    skip_without_repel_api()

    ## 60 images in one native-unit box with a small padding cannot be fully
    ## separated; the layer goes through repel_boxes() while grobbing the
    ## panel, so the partial-resolution warning must reach the user there.
    ## The default cap is used on purpose: this is the real situation.
    d <- repel_data(x = rep(0.5, 60), y = rep(0.5, 60),
                    width = 0.08, height = 0.08)
    p <- repel_plot(d, repel_geom_fun()(width = 0.08, height = 0.08,
                                        use_cache = FALSE))
    captured <- repel_collect_warnings(repel_layout_of(p))

    partial <- grep("partially resolved", captured$warnings, fixed = TRUE)
    expect_length(partial, 1L)
    ## the layout is still drawn and still finite
    expect_equal(nrow(captured$value), 60L)
    expect_true(all(is.finite(captured$value$x)))
    ## ... and the residual overlap the warning promises is real: some pair
    ## of DRAWN boxes still overlaps
    expect_gt(repel_overlap_count(captured$value), 0L)
})

test_that("the geom layer stays silent for a resolvable layout", {
    skip_without_repel_api()

    d <- repel_data(x = c(0.5, 0.5), y = c(0.5, 0.5), width = 0.2, height = 0.2)
    p <- repel_plot(d, repel_geom_fun()(width = 0.2, height = 0.2,
                                        use_cache = FALSE))
    captured <- repel_collect_warnings(repel_layout_of(p))
    expect_length(grep("partially resolved", captured$warnings, fixed = TRUE), 0L)
    expect_equal(repel_overlap_count(captured$value), 0L)
})
