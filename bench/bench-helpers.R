# Shared helpers for the ggimage developer benchmarks.
#
# These helpers deliberately depend only on base R.  The benchmark package is
# optional: when it is not installed, run_benchmark() uses system.time().

benchmark_iterations <- function(default = 3L) {
    value <- Sys.getenv("GGIMAGE_BENCH_ITERATIONS", unset = "")
    if (!nzchar(value)) return(as.integer(default))
    value <- suppressWarnings(as.integer(value))
    if (is.na(value) || value < 1L) {
        warning("GGIMAGE_BENCH_ITERATIONS must be a positive integer; using ", default)
        return(as.integer(default))
    }
    value
}

benchmark_sizes <- function(default) {
    value <- Sys.getenv("GGIMAGE_BENCH_SIZES", unset = "")
    if (!nzchar(value)) return(default)
    requested <- suppressWarnings(as.integer(trimws(strsplit(value, ",", fixed = TRUE)[[1]])))
    requested <- requested[is.finite(requested) & requested > 0L]
    selected <- default[default %in% requested]
    if (!length(selected)) {
        warning(
            "GGIMAGE_BENCH_SIZES did not select a supported size; using all defaults: ",
            paste(default, collapse = ", ")
        )
        return(default)
    }
    selected
}

run_benchmark <- function(name, expression, iterations = benchmark_iterations()) {
    if (!is.function(expression)) {
        stop("`expression` must be a function with no arguments", call. = FALSE)
    }

    if (requireNamespace("bench", quietly = TRUE)) {
        measured <- bench::mark(
            expression(),
            iterations = iterations,
            check = FALSE,
            memory = FALSE,
            time_unit = "ms"
        )
        return(data.frame(
            benchmark = name,
            median_ms = as.numeric(measured$median, units = "secs") * 1000,
            min_ms = as.numeric(measured$min, units = "secs") * 1000,
            max_ms = as.numeric(measured$max, units = "secs") * 1000,
            iterations = iterations,
            backend = "bench",
            stringsAsFactors = FALSE,
            row.names = NULL
        ))
    }

    elapsed <- numeric(iterations)
    for (i in seq_len(iterations)) {
        elapsed[[i]] <- unname(system.time(expression())[["elapsed"]])
    }
    data.frame(
        benchmark = name,
        median_ms = stats::median(elapsed) * 1000,
        min_ms = min(elapsed) * 1000,
        max_ms = max(elapsed) * 1000,
        iterations = iterations,
        backend = "base::system.time",
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

print_benchmark <- function(results) {
    print(results, row.names = FALSE)
    invisible(results)
}

find_local_image <- function(bench_dir) {
    candidates <- c(
        system.file("extdata", "Rlogo.png", package = "ggimage"),
        file.path(dirname(bench_dir), "inst", "extdata", "Rlogo.png"),
        file.path(getwd(), "inst", "extdata", "Rlogo.png")
    )
    candidates <- unique(candidates[nzchar(candidates) & file.exists(candidates)])
    if (!length(candidates)) {
        stop(
            "Could not find a local Rlogo.png. Install ggimage or run from the source checkout.",
            call. = FALSE
        )
    }
    normalizePath(candidates[[1]], winslash = "/")
}

make_unique_images <- function(local_image, n) {
    paths <- file.path(tempdir(), sprintf("ggimage-benchmark-%04d.png", seq_len(n)))
    ok <- file.copy(local_image, paths, overwrite = TRUE)
    if (!all(ok)) {
        unlink(paths[ok], force = TRUE)
        stop("Could not create temporary local image copies", call. = FALSE)
    }
    paths
}

require_benchmark_packages <- function(repel = FALSE) {
    needed <- c("ggplot2", "ggimage")
    if (repel && !exists("geom_image_repel", asNamespace("ggimage"), inherits = FALSE)) {
        stop("This ggimage build does not export geom_image_repel()", call. = FALSE)
    }
    missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
        stop(
            "Install the package(s) required by this benchmark: ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }
}

source_file_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- args[grepl("^--file=", args)]
    if (length(file_arg)) {
        return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
    }
    getwd()
}
