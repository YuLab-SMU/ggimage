##' read image (by magick::image_read) with the ability to remove marginal empty space
##'
##'
##' @title image_read2
##' @param path file path
##' @param ... additional parameters that pass to magick::image_read
##' @param cut_empty_space whether remove marginal empty space
##' @return magick-image object
##' @importFrom magick image_read
##' @importFrom magick image_info
##' @export
##' @author Guangchuang Yu
image_read2 <- function(path, ..., cut_empty_space = TRUE) {
    dots <- list(...)
    ## `geom_image()` rasterises vector documents at `.vector_density`; an
    ## image read by hand used to stay at ImageMagick's 72 dpi default.  An
    ## unnamed argument may already be the density, and naming it again would
    ## match the formal twice.
    if (length(path) == 1L && !"density" %in% names(dots) &&
        all(nzchar(names(dots))) &&
        tolower(tools::file_ext(sub("[?#].*$", "", path))) %in% c("pdf", "eps", "ps")) {
        dots$density <- .vector_density
    }
    img <- do.call(image_read, c(list(path), dots))
    if (!.image_has_frames(img)) {
        stop("ImageMagick read no frames from '", path, "'; this build has no ",
             "delegate for that format", call. = FALSE)
    }
    if (!cut_empty_space) {
        return(img)
    }

    frames <- lapply(seq_along(img), function(i) {
        .trim_white_margin(img[[i]], img[i])
    })
    if (length(frames) == 1L) {
        return(frames[[1L]])
    }
    do.call(c, frames)
}

## Trim the empty white margin off one frame.  `frame` is the raw bitmap and
## `image` the magick object it belongs to, which is returned unchanged when
## there is nothing to trim.
.trim_white_margin <- function(frame, image) {
    bitmap_dim <- dim(frame)
    n_channels <- bitmap_dim[[1L]]

    ## The first three channels define the white margin.  Build one logical
    ## mask instead of calculating row and column summaries for each channel.
    ## The channel slice is rebuilt as an explicit (second, third) margin: an
    ## image with width 1 or height 1 would otherwise collapse to a plain
    ## vector and rowSums() would abort with "'x' must be an array of at least
    ## two dimensions".
    mask_rows <- bitmap_dim[[2L]]
    mask_cols <- bitmap_dim[[3L]]
    non_white <- matrix(FALSE, nrow = mask_rows, ncol = mask_cols)
    for (channel in seq_len(min(n_channels, 3L))) {
        non_white <- non_white |
            matrix(frame[channel, , ] != as.raw(255L),
                   nrow = mask_rows, ncol = mask_cols)
    }

    row_keep <- rowSums(non_white) > 0L
    col_keep <- colSums(non_white) > 0L
    if (!any(row_keep) || !any(col_keep)) {
        ## Keep the frame for an all-white input.  Apart from being the least
        ## surprising result, this avoids invalid range(integer(0)) bounds and
        ## an unnecessary image reconstruction.
        return(image)
    }

    row_bounds <- range(which(row_keep))
    col_bounds <- range(which(col_keep))

    ## No margin means no reconstruction (and therefore no pixel conversion).
    if (identical(row_bounds, c(1L, bitmap_dim[[2L]])) &&
        identical(col_bounds, c(1L, bitmap_dim[[3L]]))) {
        return(image)
    }

    ## Crop all channels in one operation so an input matte/alpha channel is
    ## retained.  `drop = FALSE` keeps the array shape for narrow images.
    cropped <- frame[, row_bounds[[1L]]:row_bounds[[2L]],
                     col_bounds[[1L]]:col_bounds[[2L]], drop = FALSE]
    image_read(cropped)
}
