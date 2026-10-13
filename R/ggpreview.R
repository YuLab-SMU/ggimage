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
    np <- as.grob(image_read(filename))

    grid.newpage()
    grid.draw(np)
}



