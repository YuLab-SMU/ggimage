##' subview geom
##'
##' @title geom_subview
##' @param mapping aes mapping, requires 'x', 'y' and 'subview'
##' @param data data frame
##' @param width width
##' @param height height
##' @param x x position of subview. This parameter works if mapping and data is not provided
##' @param y y position of subview. This parameter works if mapping and data is not provided
##' @param subview subview to plot, if not provided in data and specify by mapping
##' @return layer
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 aes_
##' @importFrom tibble as_tibble
##' @importFrom tibble tibble
##' @importFrom ggplotify as.grob
##' @importFrom ggfun get_aes_var
##' @export
##' @author guangchuang yu
geom_subview <- function(mapping = NULL, data = NULL, width=.1, height=.1, x = NULL, y = NULL, subview = NULL) {
  ## can't support `aes(x, y, subview=subview)` as ggplot2 will throw:
  ##     cannot coerce class "c("ggtree", "gg", "ggplot")" to a data.frame

  ## this is a hack to support `aes` without `inherit.aes`. mapping and data must be provided.

  if (is.null(data)) {
    data <- tibble(x = x, y = y)
  } else if (!inherits(data, 'tbl')) {
    data <- as_tibble(data)
  }

  if (is.null(mapping)) {
    mapping <- aes_(x = ~x, y = ~y)
  }
  mapping <- as.list(mapping)
  if (is.null(mapping$x)) stop("x aesthetic mapping should be provided")
  if (is.null(mapping$y)) stop("y aesthetic mapping should be provided")
  if (is.null(mapping$subview) && is.null(subview)) stop("subview must be provided")

  if (is.null(mapping$subview)) {
    if (!inherits(subview, "list")) subview <- list(subview)
    data$subview <- subview
  } else {
    sv_var <- get_aes_var(mapping, 'subview')
    data$subview <- data[[sv_var]]
  }

  xvar <- get_aes_var(mapping, 'x')
  yvar <- get_aes_var(mapping, 'y')

  data$width <- if (is.null(mapping$width)) width else data[[get_aes_var(mapping, 'width')]]
  data$height <- if (is.null(mapping$height)) height else data[[get_aes_var(mapping, 'height')]]

  data$xmin <- data[[xvar]] - data$width/2
  data$xmax <- data[[xvar]] + data$width/2
  data$ymin <- data[[yvar]] - data$height/2
  data$ymax <- data[[yvar]] + data$height/2

  # Create a cache for as.grob results
  grob_cache <- new.env(hash = TRUE)

  lapply(1:nrow(data), function(i) {
    subview <- data$subview[[i]]

    # Check if the subview is already in the cache
    grob_key <- digest::digest(subview)  # Use digest package to create a unique key
    if (!exists(grob_key, envir = grob_cache)) {
      # If not in cache, compute and store
      grob_cache[[grob_key]] <- as.grob(subview)
    }

    # Use the cached grob
    grob <- grob_cache[[grob_key]]

    annotation_custom(
      grob,
      xmin = data$xmin[i],
      xmax = data$xmax[i],
      ymin = data$ymin[i],
      ymax = data$ymax[i])
  })
}

unit <- grid::unit
