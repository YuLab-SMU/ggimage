##' preview a plot befor saving it to a file. 
##'
##'
##' @title ggpreview
##' @param filename If it is not NULL, the previewed figure will be save to the file
##' @param plot any plot that supported by the 'ggplotify' package
##' @param width width of the figure
##' @param height height of the figure
##' @param units units of the 'width' and 'height'
##' @param ... additional parameters pass to ggsave() if filename is not NULL
##' @return a preview of the figure
##' @importFrom ggplot2 last_plot
##' @importFrom ggplot2 ggsave
##' @importFrom ggplotify as.grob
##' @importFrom grid grid.draw
##' @importFrom grid grid.newpage
##' @importFrom magick image_read
##' @export
##' @author Guangchuang Yu
ggpreview <- function(filename = NULL, plot = last_plot(), 
                    width = NA, height = NA, units = 'in', ...) {
    ## Only the file this call creates itself is removed again; a file the
    ## caller named is theirs to keep.  Without this the temporary PNG stayed
    ## in the session temp directory until the session ended.
    temporary <- is.null(filename)
    if (temporary) {
        filename <- tempfile(fileext = '.png') 
        on.exit(unlink(filename, force = TRUE), add = TRUE)
    }
    
    if (!inherits(plot, 'gg')) {
        ## plot <- as.ggplot(plot)    
        plot <- as.grob(plot)    
    }

    ggsave(filename = filename, plot = plot, 
        width = width, height = height, units = units, ...)

    ## as.ggplot(image_read(filename))
    ## Reading the file back is what shows the figure at the size it was
    ## saved at.  Where that fails -- a `device = "pdf"` save writes PDF bytes
    ## into a name ending in `.png`, and a build with no Ghostscript delegate
    ## answers such a read with an image of *no frames* rather than an error,
    ## which `as.grob()` then reports as "Image must have at least 1 frame to
    ## write a bitmap" -- preview the grob this call already has instead.
    preview <- tryCatch({
        image <- image_read(filename)
        if (!.image_has_frames(image)) {
            stop("the file was read back with no frames", call. = FALSE)
        }
        as.grob(image)
    }, error = function(e) {
        warning("ggpreview() could not re-read '", filename,
                "'; showing the plot itself: ", conditionMessage(e),
                call. = FALSE)
        plot
    })

    grid.newpage()
    grid.draw(preview)
}



