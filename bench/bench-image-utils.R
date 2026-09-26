#!/usr/bin/env Rscript

# Third-phase microbenchmarks for repeated image utility calls.
#
# All inputs are local files from the installed/source checkout. This measures
# draw_key_image() and image_read2() directly, without network requests or
# plot rendering beyond the grob construction performed by draw_key_image().

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

main <- function() {
    require_benchmark_packages()
    if (!exists("draw_key_image", envir = asNamespace("ggimage"), inherits = FALSE) ||
        !exists("image_read2", envir = asNamespace("ggimage"), inherits = FALSE)) {
        stop("This ggimage build does not expose the image utility functions", call. = FALSE)
    }

    draw_key <- getExportedValue("ggimage", "draw_key_image")
    read2 <- getExportedValue("ggimage", "image_read2")
    bench_dir <- source_file_dir()
    local_image <- find_local_image(bench_dir)
    sizes <- benchmark_sizes(c(1L, 10L, 50L, 100L))
    key_data <- data.frame(
        colour = "#3366AA",
        alpha = 0.8,
        stringsAsFactors = FALSE
    )
    old_keytype <- getOption("ggimage.keytype")
    on.exit(options(ggimage.keytype = old_keytype), add = TRUE)

    results <- list()
    k <- 0L
    for (n in sizes) {
        ## Repeated calls model repeated legend-key requests; the image key
        ## additionally exercises local image decoding on every call.
        for (keytype in c("point", "rect", "image", "blank")) {
            options(ggimage.keytype = keytype)
            label <- paste(
                "draw_key_image",
                paste0("calls=", n),
                paste0("keytype=", keytype),
                sep = " | "
            )
            expression <- local({
                key_data <- key_data
                calls <- n
                function() {
                    for (i in seq_len(calls)) {
                        draw_key(key_data, list(), 20)
                    }
                    invisible(NULL)
                }
            })
            k <- k + 1L
            results[[k]] <- run_benchmark(label, expression)
        }

        for (cut_empty_space in c(TRUE, FALSE)) {
            label <- paste(
                "image_read2",
                paste0("calls=", n),
                paste0("cut_empty_space=", cut_empty_space),
                sep = " | "
            )
            expression <- local({
                path <- local_image
                trim <- cut_empty_space
                calls <- n
                function() {
                    for (i in seq_len(calls)) {
                        read2(path, cut_empty_space = trim)
                    }
                    invisible(NULL)
                }
            })
            k <- k + 1L
            results[[k]] <- run_benchmark(label, expression)
        }
    }

    result <- do.call(rbind, results)
    cat("# image utility microbenchmarks (local image; no network)\n")
    cat("# Set GGIMAGE_BENCH_ITERATIONS and GGIMAGE_BENCH_SIZES to tune a run.\n")
    print_benchmark(result)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
