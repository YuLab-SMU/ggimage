#!/usr/bin/env Rscript

# First-phase performance tracking for geom_image().
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
    require_benchmark_packages()
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required", call. = FALSE)
    }

    bench_dir <- source_file_dir()
    local_image <- find_local_image(bench_dir)
    sizes <- benchmark_sizes(c(1L, 10L, 100L, 1000L))
    unique_images <- make_unique_images(local_image, max(sizes))
    on.exit(unlink(unique_images, force = TRUE), add = TRUE)

    results <- vector("list", length(sizes) * 2L * 2L * 2L)
    k <- 0L
    for (n in sizes) {
        grid_n <- ceiling(sqrt(n))
        coordinates <- seq(0.05, 0.95, length.out = grid_n)
        data_template <- data.frame(
            x = rep(coordinates, length.out = n),
            y = rep(coordinates, each = grid_n, length.out = n)
        )

        for (image_set in c("repeated", "unique")) {
            images <- if (image_set == "repeated") {
                rep(local_image, n)
            } else {
                unique_images[seq_len(n)]
            }
            data <- cbind(data_template, image = images)

            for (cache in c(TRUE, FALSE)) {
                for (sizing in c("size", "explicit")) {
                    k <- k + 1L
                    label <- paste(
                        "geom_image",
                        paste0("n=", n),
                        paste0("images=", image_set),
                        paste0("cache=", cache),
                        paste0("sizing=", sizing),
                        sep = " | "
                    )
                    expression <- local({
                        d <- data
                        use_cache <- cache
                        sizing <- sizing
                        function() {
                            layer <- if (sizing == "size") {
                                ggimage::geom_image(size = 0.05, use_cache = use_cache)
                            } else {
                                ggimage::geom_image(
                                    width = 0.03,
                                    height = 0.03,
                                    use_cache = use_cache
                                )
                            }
                            plot <- ggplot2::ggplot(d, ggplot2::aes(x, y, image = image)) + layer
                            ggplot2::ggplotGrob(plot)
                            invisible(NULL)
                        }
                    })
                    results[[k]] <- run_benchmark(label, expression)
                }
            }
        }
    }

    result <- do.call(rbind, results)
    cat("# geom_image benchmark (local files; ggplotGrob build)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS to change repetitions.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
