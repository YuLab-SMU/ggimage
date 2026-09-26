#!/usr/bin/env Rscript

# Third-phase scaling benchmark for the repel broad phase.
#
# Inputs are explicit boxes, so this isolates sparse/dense candidate scaling
# from image decoding and ggplot2 grob construction. The benchmark remains
# useful with the current pairwise solver and can be compared with a future
# broad-phase implementation without changing package runtime code.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

repel_inputs <- function(n, density) {
    grid_n <- ceiling(sqrt(n))
    if (density == "dense") {
        ## Adjacent boxes overlap in a compact, deterministic cluster while
        ## retaining a bounded coordinate range for the largest case.
        span <- max(0.02, min(0.10, 0.12 * sqrt(n / 1000)))
        coordinates <- seq(0.5 - span / 2, 0.5 + span / 2,
                          length.out = grid_n)
        box_size <- 0.01
    } else {
        ## The sparse lattice keeps the same ordering but leaves boxes mostly
        ## independent, which is the useful contrast for a broad phase.
        coordinates <- seq(0.05, 0.95, length.out = grid_n)
        box_size <- 0.01
    }

    list(
        x = rep(coordinates, length.out = n),
        y = rep(coordinates, each = grid_n, length.out = n),
        width = rep(box_size, n),
        height = rep(box_size, n)
    )
}

main <- function() {
    require_benchmark_packages(repel = TRUE)
    solver <- get0("repel_boxes", envir = asNamespace("ggimage"),
                   inherits = FALSE)
    if (!is.function(solver)) {
        stop("This ggimage build does not expose the repel solver", call. = FALSE)
    }

    ## Larger sizes than bench-repel-solver.R make scaling visible while the
    ## default max.iter=1 keeps this phase focused on pair/candidate discovery.
    sizes <- benchmark_sizes(c(128L, 256L, 512L, 1024L))
    results <- vector("list", length(sizes) * 2L)
    k <- 0L

    for (n in sizes) {
        for (density in c("dense", "sparse")) {
            inputs <- repel_inputs(n, density)
            label <- paste(
                "repel broad-phase",
                paste0("n=", n),
                paste0("density=", density),
                "max.iter=1",
                sep = " | "
            )
            expression <- local({
                x <- inputs$x
                y <- inputs$y
                width <- inputs$width
                height <- inputs$height
                function() {
                    solver(x = x, y = y, width = width, height = height,
                           max.iter = 1L, force = 0.1, direction = "both")
                    invisible(NULL)
                }
            })
            k <- k + 1L
            results[[k]] <- run_benchmark(label, expression)
        }
    }

    result <- do.call(rbind, results)
    cat("# repel broad-phase scaling (explicit boxes; no images or grobs)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS and GGIMAGE_BENCH_SIZES to tune a run.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
