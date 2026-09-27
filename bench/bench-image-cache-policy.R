#!/usr/bin/env Rscript

# Fourth-phase offline cache policy benchmark.
# Exercises byte-cap eviction and hit/miss diagnostics without network access.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

main <- function() {
    require_benchmark_packages()
    ns <- asNamespace("ggimage")
    needed <- c("clear_image_cache", "prepare_image", "set_image_cache_policy",
                "get_image_cache_stats")
    functions <- setNames(lapply(needed, get0, envir = ns, inherits = FALSE), needed)
    if (!all(vapply(functions, is.function, logical(1)))) {
        cat("# cache-policy benchmark skipped: policy API unavailable\n")
        return(invisible(NULL))
    }

    local_image <- find_local_image(source_file_dir())
    sizes <- benchmark_sizes(c(4L, 8L, 16L))
    generated <- make_unique_images(local_image, max(sizes))
    on.exit(unlink(generated, force = TRUE), add = TRUE)

    old <- options(
        ggimage.image_cache_capacity = 2,
        ggimage.image_cache_transform_capacity = 2,
        ggimage.image_cache_bytes = Inf,
        ggimage.image_cache_ttl = Inf,
        ggimage.image_cache_eviction = "lru"
    )
    on.exit(options(old), add = TRUE)

    rows <- lapply(sizes, function(n) {
        functions$clear_image_cache()
        for (image in generated[seq_len(n)]) {
            functions$prepare_image(image, NULL, 1, 0, NULL, use_cache = TRUE)
        }
        stats <- functions$get_image_cache_stats()
        data.frame(
            n = n,
            base_entries = unname(stats$base["entries"]),
            transform_entries = unname(stats$transform["entries"]),
            base_misses = unname(stats$base["misses"]),
            evictions = unname(stats$base["evictions"]),
            status = if (stats$base["entries"] <= 2 && stats$base["evictions"] > 0) "ok" else "FAIL",
            row.names = NULL
        )
    })
    result <- do.call(rbind, rows)
    cat("# fourth-phase image cache policy benchmark (offline)\n")
    print(result, row.names = FALSE)
    if (any(result$status != "ok")) stop("cache policy benchmark failed", call. = FALSE)
    invisible(result)
}

if (identical(environment(), globalenv())) main()
