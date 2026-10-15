## Platform-capability helpers for the format readers.
##
## A build of ImageMagick with no delegate for a format does not always raise.
## Some hosts (notably the CRAN macOS arm64 check machine, which has no
## Ghostscript) answer `image_read()` of a `.pdf` with a magick object that
## carries *no frames at all*.  That failure only surfaces later -- drawing it
## aborts with "Image must have at least 1 frame to write a bitmap" -- so a
## test that skips "no rasteriser on this machine" must treat both spellings
## as the same thing: this machine cannot read this format.
can_read <- function(expr) {
    result <- tryCatch(expr, error = function(e) NULL)
    !is.null(result) && length(result) > 0L
}
