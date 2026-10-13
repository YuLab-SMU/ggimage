##' set background for ggplot
##'
##'
##' @title ggbackground
##' @param gg gg object
##' @param background background image
##' @param ... additional parameter to manipulate background image, see also geom_image
##' @return gg object, with the background as its first layer. The layer sits on
##' one of the observations the plot already has, so it does not widen the axis
##' ranges and needs no continuous axis.
##' @importFrom ggplot2 ggplot
##' @export
##' @author guangchuang yu
ggbackground <- function(gg, background, ...) {
    ## Add an isolated image layer as the first layer instead of wrapping `gg`
    ## in an annotation_custom grob.  Wrapping replaces the plot's coordinate
    ## system with the helper plot's 0:1 ranges, so annotations added after
    ## ggbackground() can be transformed to the wrong position (or clipped).
    ##
    ## The layer must not inherit the plot's global aesthetics: a mapping such
    ## as colour/alpha would otherwise be applied to the background once per
    ## observation and could tint or duplicate the image.
    background_layer <- geom_image(
        data = ggbackground_anchor(gg),
        mapping = ggplot2::aes(
            x = !!rlang::sym("x"),
            y = !!rlang::sym("y")
        ),
        inherit.aes = FALSE,
        image = background,
        size = Inf,
        ...
    )
    gg$layers <- c(list(background_layer), gg$layers)
    gg
}

## The image fills the panel in npc and its x/y are never read, but the layer
## still needs one row, and that row trains the scales: fixed values can extend
## the panel range and cannot be mapped onto a discrete axis at all.  One of the
## plot's own observations is therefore used, when it can be found.
ggbackground_anchor <- function(gg) {
    fallback <- data.frame(x = 0.5, y = 0.5)
    x_name <- ggbackground_column(gg$mapping$x)
    y_name <- ggbackground_column(gg$mapping$y)
    if (is.null(x_name) || is.null(y_name)) {
        return(fallback)
    }
    candidates <- c(list(gg$data), lapply(gg$layers, function(l) l$data))
    for (candidate in candidates) {
        if (!is.data.frame(candidate) || !nrow(candidate)) next
        if (!(x_name %in% names(candidate) && y_name %in% names(candidate))) next
        x_value <- ggbackground_first(candidate[[x_name]])
        y_value <- ggbackground_first(candidate[[y_name]])
        if (!is.null(x_value) && !is.null(y_value)) {
            return(data.frame(x = x_value, y = y_value))
        }
    }
    fallback
}

ggbackground_column <- function(mapping) {
    ## `aes()` stores its bindings as quosures since ggplot2 3.4, and a quosure
    ## is a formula, not the symbol it wraps, so the symbol has to be taken out
    ## before it can name a column.
    if (rlang::is_quosure(mapping)) {
        mapping <- rlang::quo_get_expr(mapping)
    }
    if (!is.symbol(mapping)) return(NULL)
    as.character(mapping)
}

ggbackground_first <- function(column) {
    position <- which(!is.na(column))
    if (!length(position)) return(NULL)
    column[[position[[1L]]]]
}
