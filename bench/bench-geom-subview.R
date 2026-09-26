#!/usr/bin/env Rscript

# Third-phase benchmark for repeated geom_subview rendering.
#
# Each subview is a small, deterministic ggplot object. The benchmark builds a
# parent ggplot containing n local subviews and materializes its ggplotGrob;
# no external assets or network requests are involved.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

main <- function() {
    require_benchmark_packages()
    if (!exists("geom_subview", envir = asNamespace("ggimage"), inherits = FALSE)) {
        cat("# geom_subview benchmark skipped: geom_subview() is unavailable\n")
        return(invisible(NULL))
    }

    sizes <- benchmark_sizes(c(1L, 5L, 10L, 25L))
    subview_data <- data.frame(x = 1:3, y = c(1, 3, 2))
    subview <- ggplot2::ggplot(subview_data, ggplot2::aes(x, y)) +
        ggplot2::geom_line(linewidth = 0.3) +
        ggplot2::geom_point(size = 0.8) +
        ggplot2::theme_void()

    results <- vector("list", length(sizes))
    for (i in seq_along(sizes)) {
        n <- sizes[[i]]
        coordinates <- data.frame(
            x = seq(0.08, 0.92, length.out = n),
            y = seq(0.92, 0.08, length.out = n)
        )
        subviews <- rep(list(subview), n)
        label <- paste("geom_subview", paste0("n=", n), sep = " | ")
        expression <- local({
            d <- coordinates
            views <- subviews
            function() {
                plot <- ggplot2::ggplot() +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1) +
                    ggimage::geom_subview(
                        data = d,
                        mapping = ggplot2::aes(x = x, y = y),
                        width = 0.12,
                        height = 0.12,
                        subview = views
                    ) +
                    ggplot2::theme_void()
                ggplot2::ggplotGrob(plot)
                invisible(NULL)
            }
        })
        results[[i]] <- run_benchmark(label, expression)
    }

    result <- do.call(rbind, results)
    cat("# geom_subview benchmark (repeated local ggplot subviews)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS and GGIMAGE_BENCH_SIZES to tune a run.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
