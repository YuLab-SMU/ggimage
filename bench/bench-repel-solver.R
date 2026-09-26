#!/usr/bin/env Rscript

# Second-phase isolated performance tracking for the repel solver.
#
# This bypasses ggplot2, image decoding, and grob construction so changes to
# the pairwise layout solver can be compared separately. The inputs are
# deterministic and offline; see bench/README.md for usage.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

repel_inputs <- function(n, density) {
    if (density == "dense") {
        ## Keep a compact but bounded cloud. The width is chosen so that the
        ## dense cases contain overlaps without forcing an unbounded cleanup
        ## run for the largest tracked size.
        grid_n <- ceiling(sqrt(n))
        span <- max(0.02, min(0.10, 0.12 * sqrt(n / 250)))
        coordinates <- seq(0.5 - span / 2, 0.5 + span / 2,
                          length.out = grid_n)
    } else {
        ## The sparse lattice has the same deterministic ordering but leaves
        ## enough panel space for boxes to remain mostly independent.
        grid_n <- ceiling(sqrt(n))
        coordinates <- seq(0.05, 0.95, length.out = grid_n)
    }

    list(
        x = rep(coordinates, length.out = n),
        y = rep(coordinates, each = grid_n, length.out = n),
        width = rep(0.01, n),
        height = rep(0.01, n)
    )
}

main <- function() {
    require_benchmark_packages(repel = TRUE)
    solver <- get0("repel_boxes", envir = asNamespace("ggimage"),
                   inherits = FALSE)
    if (!is.function(solver)) {
        stop("This ggimage build does not expose the repel solver", call. = FALSE)
    }

    sizes <- benchmark_sizes(c(10L, 50L, 100L, 250L))
    max_iterations <- c(1L, 10L, 100L)
    results <- vector("list", length(sizes) * 2L * length(max_iterations))
    k <- 0L

    for (n in sizes) {
        for (density in c("dense", "sparse")) {
            inputs <- repel_inputs(n, density)
            for (max.iter in max_iterations) {
                k <- k + 1L
                label <- paste(
                    "repel_boxes",
                    paste0("n=", n),
                    paste0("density=", density),
                    paste0("max.iter=", max.iter),
                    sep = " | "
                )
                expression <- local({
                    x <- inputs$x
                    y <- inputs$y
                    width <- inputs$width
                    height <- inputs$height
                    iterations <- max.iter
                    function() {
                        solver(x = x, y = y, width = width, height = height,
                               max.iter = iterations, force = 0.1,
                               direction = "both")
                        invisible(NULL)
                    }
                })
                results[[k]] <- run_benchmark(label, expression)
            }
        }
    }

    result <- do.call(rbind, results)
    cat("# isolated repel solver benchmark (deterministic; no images or grobs)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS and GGIMAGE_BENCH_SIZES to tune a run.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
