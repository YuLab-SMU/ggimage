#!/usr/bin/env Rscript

# Profile image fingerprinting for a local animated GIF. Supply the GIF path as
# the first argument; defaults to the bundled still image for smoke testing.
args <- commandArgs(trailingOnly = TRUE)
script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- script_args[grepl("^--file=", script_args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

if (!requireNamespace("magick", quietly = TRUE)) stop("Install magick first")
if (!requireNamespace("digest", quietly = TRUE)) stop("Install digest first")
image_path <- if (length(args)) args[[1L]] else file.path(repo_root, "inst", "extdata", "Rlogo.png")
if (!file.exists(image_path)) stop("Image not found: ", image_path)
image_path <- normalizePath(image_path)

source_expr <- parse(file.path(repo_root, "R", "geom_image.R"))
key_expr <- source_expr[[which(vapply(source_expr, function(expr) {
    is.call(expr) && identical(as.character(expr[[1L]]), "<-") &&
        identical(as.character(expr[[2L]]), "magick_image_key")
}, logical(1)))]]
key_env <- new.env(parent = asNamespace("ggimage"))
eval(key_expr, envir = key_env)
image_key <- key_env$magick_image_key

cat("Image:", image_path, "\n")
read_time <- system.time(image <- magick::image_read(image_path))[["elapsed"]]
info <- magick::image_info(image)
cat("Read:", round(read_time * 1000, 1), "ms; frames:", nrow(info),
    "; dimensions:", paste(unique(info$width), collapse = ","), "x",
    paste(unique(info$height), collapse = ","), "\n")

frame_export <- function() {
    lapply(seq_len(nrow(info)), function(i) magick::image_data(image[i]))
}
frame_digest <- function() {
    blobs <- frame_export()
    vapply(blobs, function(x) digest::digest(as.raw(x), algo = "sha1", serialize = FALSE), character(1))
}

measure <- function(label, f, iterations = 3L) {
    times <- replicate(iterations, unname(system.time(f())[["elapsed"]]))
    data.frame(stage = label, median_ms = round(stats::median(times) * 1000, 1),
               min_ms = round(min(times) * 1000, 1),
               max_ms = round(max(times) * 1000, 1))
}

results <- do.call(rbind, list(
    measure("image_data per frame (export)", frame_export),
    measure("pixel digest per frame", frame_digest),
    measure("full magick_image_key", function() image_key(image))
))
cat("\nWarm-cache stage timings (three runs):\n")
print(results, row.names = FALSE)
cat("\nNote: timings exclude GIF file read except the separate read measurement.\n")
