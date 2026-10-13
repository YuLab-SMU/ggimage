## Helpers for inspecting a rendered gtable.

## Collect every grob in a gtable/ggplot object that satisfies `predicate`.
## The traversal descends into nested gtables as well as grob children, because
## ggplot2 stores each layer's output inside the panel's gTree and the panel
## itself is a grob of the outer gtable.
collect_grobs <- function(x, predicate) {
    found <- list()
    visit <- function(node) {
        if (is.null(node)) return(invisible(NULL))
        if (predicate(node)) found[[length(found) + 1L]] <<- node
        if (inherits(node, "gtable")) {
            for (child in node$grobs) visit(child)
        }
        if (!is.null(node$children)) {
            for (child in node$children) visit(child)
        }
        invisible(NULL)
    }
    visit(x)
    found
}

## Raster grobs in draw order.  ggplot2 draws the layers in the order they were
## added, so element i of the result belongs to row i of the layer data for a
## one-layer plot.
find_raster_grobs <- function(x) {
    collect_grobs(x, function(g) inherits(g, "rastergrob"))
}
