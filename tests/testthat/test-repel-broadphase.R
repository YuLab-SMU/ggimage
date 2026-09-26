test_that("broad-phase candidates match the full scan on reference fixtures", {
    solver <- function(args, broad.phase) {
        do.call(ggimage:::repel_boxes,
                c(args, list(broad.phase = broad.phase)))
    }

    fixtures <- list(
        list(x = c(0.5, 0.5), y = c(0.5, 0.5),
             width = c(0.2, 0.2), height = c(0.1, 0.3),
             max.iter = 7L, force = 0.2, direction = "both"),
        list(x = c(0.2, 0.25, 0.8, 0.81, 0.5),
             y = c(0.5, 0.5, 0.51, 0.5, 0.1),
             width = c(0.1, 0.2, 0.05, 0.15, 0.4),
             height = c(0.2, 0.1, 0.3, 0.2, 0.05),
             max.iter = 5L, force = 1, direction = "x"),
        list(x = seq(0.2, 0.8, length.out = 12),
             y = c(rep(0.5, 6), rep(0.52, 6)),
             width = rep(c(0.08, 0.13), 6),
             height = rep(c(0.12, 0.07), 6),
             max.iter = 9L, force = 0.1, direction = "y")
    )

    for (args in fixtures) {
        reference <- solver(args, broad.phase = FALSE)
        candidate <- solver(args, broad.phase = TRUE)
        expect_equal(candidate, reference, tolerance = 1e-12)
    }

    ## This fixture crosses the production threshold and includes both a
    ## coincident cluster and boxes with unequal extents. It checks the actual
    ## sweep-and-prune path against the same full-scan reference.
    n <- 96L
    args <- list(
        x = c(rep(0.5, 6), seq(0, 20, length.out = n - 6L)),
        y = c(rep(0.5, 6), rep(c(0, 1), length.out = n - 6L)),
        width = rep(seq(0.08, 0.2, length.out = n), length.out = n),
        height = rep(seq(0.05, 0.16, length.out = n), length.out = n),
        max.iter = 5L, force = 0.2, direction = "both"
    )
    reference <- solver(args, broad.phase = FALSE)
    candidate <- solver(args, broad.phase = TRUE)
    expect_equal(candidate, reference, tolerance = 1e-12)
})


test_that("broad phase handles sparse layouts at n = 100 and 250", {
    for (n in c(100L, 250L)) {
        ## Most boxes are far apart, while the first few form a deterministic
        ## local conflict. This exercises both the empty-candidate and active
        ## sweep paths without making a wall-clock claim.
        x <- seq(0, by = 2, length.out = n)
        y <- rep(c(0, 1), length.out = n)
        x[seq_len(6L)] <- c(0, 0, 0.04, 0.08, 0.12, 0.16)
        y[seq_len(6L)] <- c(0, 0, 0, 0.04, 0.08, 0.12)
        args <- list(
            x = x, y = y,
            width = rep(0.2, n), height = rep(0.2, n),
            max.iter = 5L, force = 0.2, direction = "both"
        )

        reference <- do.call(ggimage:::repel_boxes,
                             c(args, list(broad.phase = FALSE)))
        first <- do.call(ggimage:::repel_boxes,
                         c(args, list(broad.phase = TRUE)))
        second <- do.call(ggimage:::repel_boxes,
                          c(args, list(broad.phase = TRUE)))

        expect_equal(first, reference, tolerance = 1e-12)
        expect_equal(second, first)
        expect_length(first$x, n)
        expect_true(all(is.finite(first$x)) && all(is.finite(first$y)))
        expect_equal(first$x[7:n], x[7:n])
        expect_equal(first$y[7:n], y[7:n])
    }
})
