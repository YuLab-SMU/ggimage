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
    img <- image_read(path, ...)
    if (!cut_empty_space) {
        return(img)
    }

    bitmap <- img[[1]]
    bitmap_dim <- dim(bitmap)
    n_channels <- bitmap_dim[[1L]]

    ## The first three channels define the white margin.  Build one logical
    ## mask instead of calculating row and column summaries for each channel;
    ## this also avoids copying each cropped channel into a second array.
    non_white <- bitmap[1L,,] != as.raw(255L)
    if (n_channels >= 2L) {
        non_white <- non_white | bitmap[2L,,] != as.raw(255L)
    }
    if (n_channels >= 3L) {
        non_white <- non_white | bitmap[3L,,] != as.raw(255L)
    }

    row_keep <- rowSums(non_white) > 0L
    col_keep <- colSums(non_white) > 0L
    if (!any(row_keep) || !any(col_keep)) {
        ## Keep the original image for an all-white input.  Apart from being
        ## the least surprising result, this avoids invalid range(integer(0))
        ## bounds and an unnecessary image reconstruction.
        return(img)
    }

    row_bounds <- range(which(row_keep))
    col_bounds <- range(which(col_keep))

    ## No margin means no reconstruction (and therefore no pixel conversion).
    if (identical(row_bounds, c(1L, bitmap_dim[[2L]])) &&
        identical(col_bounds, c(1L, bitmap_dim[[3L]]))) {
        return(img)
    }

    ## Crop all channels in one operation so an input matte/alpha channel is
    ## retained.  `drop = FALSE` keeps the array shape for narrow images.
    cropped <- bitmap[, row_bounds[[1L]]:row_bounds[[2L]],
                      col_bounds[[1L]]:col_bounds[[2L]], drop = FALSE]
    image_read(cropped)
}
