##' set background for ggplot
##'
##'
##' @title ggbackground
##' @param gg gg object
##' @param background background image
##' @param ... additional parameter to manipulate background image, see also geom_image
##' @return gg object, with the background as its first layer. The layer sits on
##' one of the observations the plot already has, so it does not widen the axis
##' ranges and needs no continuous axis. The observation is taken from the
##' plot-level `aes()`, or from the first layer that maps `x` and `y`.
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

## The image fills the panel in npc and its x/y are never read -- with
## `size = Inf`, `imageGrob()` replaces them with the panel centre, so only the
## training of the scales depends on the values -- but the layer still needs one
## row, and that row trains the scales: fixed values can extend the panel range
## and cannot be mapped onto a discrete axis at all.  One of the plot's own
## observations is therefore used, when it can be found.
##
## The columns are looked up in the plot-level mapping first and then in each
## layer's own mapping.  Reading `gg$mapping` alone silently missed the common
## `ggplot() + geom_point(data = d, aes(x, y))` idiom, where the aesthetics
## never reach the plot: the code then fell back to the hard-coded `(0.5, 0.5)`
## row, which widened the axis ranges, drew a phantom point at the panel corner
## and aborted outright on a discrete axis.  Scanning the layers is preferred
## over the `annotation_raster(-Inf, Inf, ...)` trick `geom_bgimage()` uses
## because it keeps the background a `geom_image()` layer, exactly as before,
## so plots that already work are unchanged -- that is also why the plot-level
## mapping stays the first choice.
ggbackground_anchor <- function(gg) {
    candidates <- c(list(gg$data), lapply(gg$layers, function(l) l$data))
    for (mapping in ggbackground_mappings(gg)) {
        for (candidate in candidates) {
            if (!is.data.frame(candidate) || !nrow(candidate)) next
            if (!all(mapping %in% names(candidate))) next
            x_value <- ggbackground_first(candidate[[mapping[[1L]]]])
            y_value <- ggbackground_first(candidate[[mapping[[2L]]]])
            if (!is.null(x_value) && !is.null(y_value)) {
                return(data.frame(x = x_value, y = y_value))
            }
        }
    }
    ## nothing usable was found: keep the historical data point so that a plot
    ## without any x/y mapping still draws its background
    data.frame(x = 0.5, y = 0.5)
}

## The `c(x, y)` column names a mapping names, if it names any: the plot-level
## mapping first, then every layer's own mapping in draw order.
ggbackground_mappings <- function(gg) {
    mappings <- c(list(gg$mapping), lapply(gg$layers, function(l) l$mapping))
    out <- list()
    for (mapping in mappings) {
        x_name <- ggbackground_column(mapping$x)
        y_name <- ggbackground_column(mapping$y)
        if (!is.null(x_name) && !is.null(y_name)) {
            out[[length(out) + 1L]] <- c(x_name, y_name)
        }
    }
    out
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
