#!/usr/bin/env Rscript

# First-phase performance tracking for geom_image_repel().
#
# Offline by default: every image is a local file.  See bench/README.md for
# installation and output guidance.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

main <- function() {
    require_benchmark_packages(repel = TRUE)

    bench_dir <- source_file_dir()
    local_image <- find_local_image(bench_dir)
    sizes <- benchmark_sizes(c(10L, 50L, 100L, 250L))
    max_iterations <- c(1L, 10L, 100L)

    results <- vector("list", length(sizes) * 2L * length(max_iterations))
    k <- 0L
    for (n in sizes) {
        for (density in c("dense", "sparse")) {
            data <- if (density == "dense") {
                ## A compact cloud creates many pairwise overlaps.
                i <- seq_len(n)
                data.frame(
                    x = 0.5 + ((i - 1L) %% 25L - 12L) * 0.0015,
                    y = 0.5 + ((i - 1L) %/% 25L - ceiling(n / 25) / 2) * 0.0015
                )
            } else {
                ## A deterministic lattice spreads points across the panel.
                grid_n <- ceiling(sqrt(n))
                coordinates <- seq(0.05, 0.95, length.out = grid_n)
                data.frame(
                    x = rep(coordinates, length.out = n),
                    y = rep(coordinates, each = grid_n, length.out = n)
                )
            }
            data$image <- rep(local_image, n)

            for (max.iter in max_iterations) {
                k <- k + 1L
                label <- paste(
                    "geom_image_repel",
                    paste0("n=", n),
                    paste0("density=", density),
                    paste0("max.iter=", max.iter),
                    sep = " | "
                )
                expression <- local({
                    d <- data
                    iterations <- max.iter
                    function() {
                        plot <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) +
                            ggimage::geom_image_repel(
                                width = 0.04,
                                height = 0.04,
                                box.padding = 0.002,
                                max.iter = iterations,
                                use_cache = TRUE
                            )
                        ggplot2::ggplotGrob(plot)
                        invisible(NULL)
                    }
                })
                results[[k]] <- run_benchmark(label, expression)
            }
        }
    }

    result <- do.call(rbind, results)
    cat("# geom_image_repel benchmark (local files; ggplotGrob build)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS to change repetitions.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
