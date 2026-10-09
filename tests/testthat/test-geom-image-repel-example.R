## The `@examples` of geom_image_repel() are the documentation users copy. They
## claimed "overlapping images are pushed apart" while the default axis limits
## spread the three points over the whole panel, so nothing ever overlapped and
## the repelling calls were indistinguishable from max.iter = 0. Assert on the
## example itself so it cannot silently become a no-op again.

repel_example_plots <- function() {
    rd <- pkg_rd_file("geom_image_repel")
    if (is.null(rd)) return(NULL)
    ex <- tempfile(fileext = ".R")
    on.exit(unlink(ex), add = TRUE)
    tools::Rd2ex(rd, ex)
    env <- new.env(parent = globalenv())
    plots <- list()
    for (expr in parse(ex)) {
        is_assignment <- is.call(expr) &&
            as.character(expr[[1L]]) %in% c("<-", "=", "assign")
        value <- eval(expr, env)
        if (!is_assignment && inherits(value, "ggplot")) {
            plots[[length(plots) + 1L]] <- value
        }
    }
    plots
}

test_that("the documented example really overlaps and really repels", {
    plots <- repel_example_plots()
    if (is.null(plots)) skip("man/geom_image_repel.Rd not found")

    expect_length(plots, 4L)

    layouts <- lapply(plots, repel_layout_of)

    ## 1. the premise: geom_image() stacks the images on top of each other
    expect_gt(repel_overlap_count(layouts[[1]]), 0L)

    ## 2. geom_image_repel() separates every pair
    expect_equal(repel_overlap_count(layouts[[2]]), 0L)
    expect_true(all(repel_pair_gaps(layouts[[2]])$gap > 0))
    expect_gt(min(repel_pair_gaps(layouts[[2]])$gap),
              min(repel_pair_gaps(layouts[[1]])$gap))

    ## 3. max.iter = 0 is documented to keep the original coordinates
    expect_equal(layouts[[3]]$x, layouts[[1]]$x)
    expect_equal(layouts[[3]]$y, layouts[[1]]$y)

    ## 4. direction = "x" is documented to move them apart horizontally only
    expect_equal(sort(layouts[[4]]$y), sort(layouts[[1]]$y))
    expect_false(isTRUE(all.equal(sort(layouts[[4]]$x), sort(layouts[[1]]$x))))
    expect_equal(repel_overlap_count(layouts[[4]]), 0L)
})
