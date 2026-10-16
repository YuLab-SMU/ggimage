#!/usr/bin/env Rscript

# Compare stable content-key costs on an existing magick image. This isolates
# the per-frame fingerprint from file reading and plot drawing. The installed
# ggimage may be older than the source checkout, so load the checked-out key
# function into a private environment for a valid before-install benchmark.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
helper <- file.path(if (nzchar(script_path)) dirname(script_path) else getwd(), "bench-helpers.R")
if (!file.exists(helper)) helper <- file.path(getwd(), "bench", "bench-helpers.R")
source(helper)

require_benchmark_packages()
ns <- asNamespace("ggimage")
source_expr <- parse(file.path(dirname(source_file_dir()), "R", "geom_image.R"))
key_expr <- source_expr[[which(vapply(source_expr, function(expr) {
    is.call(expr) && identical(as.character(expr[[1L]]), "<-") &&
        identical(as.character(expr[[2L]]), "magick_image_key")
}, logical(1)))]]
key_env <- new.env(parent = ns)
eval(key_expr, envir = key_env)
image_key <- key_env$magick_image_key
image_path <- find_local_image(source_file_dir())
image <- magick::image_read(image_path)

# Reproduce the pre-0.3.8 key operation for comparison; unlike the current
# implementation this hashes encoded PNG bytes and is therefore not stable
# across ImageMagick timestamp boundaries.
legacy_key <- function(x) digest::digest(magick::image_write(x, format = "png"),
                                         algo = "sha1")

results <- rbind(
    run_benchmark("current pixel fingerprint", function() image_key(image)),
    run_benchmark("legacy PNG-byte fingerprint", function() legacy_key(image)),
    run_benchmark("image_data only", function() magick::image_data(image))
)
cat("# magick image key cost (one local image, no file IO)\n")
print_benchmark(results)
