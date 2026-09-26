#!/usr/bin/env Rscript

# Offline cache policy smoke checks for the image preparation path.
#
# The checks intentionally inspect the package's internal cache counters rather
# than timing cache hits. They use generated local copies of the bundled image,
# so no network or external asset is involved. See bench/README.md.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

cache_functions <- function() {
    namespace <- asNamespace("ggimage")
    names <- c("clear_image_cache", "get_image_cache_size",
               "get_image_transform_cache_size", "prepare_image")
    setNames(lapply(names, function(name) {
        get0(name, envir = namespace, inherits = FALSE)
    }), names)
}

prepare_images <- function(prepare_image, images, use_cache) {
    for (image in images) {
        prepare_image(
            img = image,
            colour = NULL,
            opacity = 1,
            angle = 0,
            image_fun = NULL,
            use_cache = use_cache
        )
    }
    invisible(NULL)
}

cache_row <- function(policy, n, stats, expected_base, expected_transform) {
    base <- as.integer(stats$get_image_cache_size())
    transform <- as.integer(stats$get_image_transform_cache_size())
    ok <- identical(base, as.integer(expected_base)) &&
        identical(transform, as.integer(expected_transform))
    data.frame(
        policy = policy,
        n = as.integer(n),
        base_cache = base,
        transform_cache = transform,
        expected_base = as.integer(expected_base),
        expected_transform = as.integer(expected_transform),
        status = if (ok) "ok" else "FAIL",
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

main <- function() {
    require_benchmark_packages()
    stats <- cache_functions()
    if (!all(vapply(stats, is.function, logical(1)))) {
        cat("# cache smoke skipped: internal cache counters are unavailable\n")
        return(invisible(NULL))
    }

    bench_dir <- source_file_dir()
    local_image <- find_local_image(bench_dir)
    sizes <- benchmark_sizes(c(10L, 50L, 100L, 250L))
    generated_images <- make_unique_images(local_image, max(sizes))
    on.exit(unlink(generated_images, force = TRUE), add = TRUE)

    rows <- vector("list", length(sizes) * 3L)
    k <- 0L
    for (n in sizes) {
        ## Repeated paths should reuse one base and one transformed entry.
        stats$clear_image_cache()
        prepare_images(stats$prepare_image, rep(local_image, n), use_cache = TRUE)
        k <- k + 1L
        rows[[k]] <- cache_row("repeated/use_cache=TRUE", n, stats, 1L, 1L)

        ## Generated unique paths should grow both caches with n.
        stats$clear_image_cache()
        prepare_images(stats$prepare_image, generated_images[seq_len(n)],
                       use_cache = TRUE)
        k <- k + 1L
        rows[[k]] <- cache_row("unique/use_cache=TRUE", n, stats, n, n)

        ## Disabling caching must not populate either internal cache.
        stats$clear_image_cache()
        prepare_images(stats$prepare_image, generated_images[seq_len(n)],
                       use_cache = FALSE)
        k <- k + 1L
        rows[[k]] <- cache_row("unique/use_cache=FALSE", n, stats, 0L, 0L)
    }

    result <- do.call(rbind, rows)
    cat("# image cache growth/policy smoke (local generated files)\n")
    print(result, row.names = FALSE)
    if (any(result$status != "ok")) {
        stop("image cache policy smoke check failed", call. = FALSE)
    }
    invisible(result)
}

if (identical(environment(), globalenv())) main()
