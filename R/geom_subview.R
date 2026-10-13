## Convert only stable, local subview objects. Functions/formulas and other
## classes may have side effects when printed, so leave those uncached.
.subview_cache_key <- function(subview) {
    if (!inherits(subview, c("grob", "ggplot", "magick-image"))) {
        return(NULL)
    }
    if (inherits(subview, "magick-image")) {
        ## digest() of an image object hashes its pointer, not its pixels
        key <- magick_image_key(subview)
        if (is.null(key)) return(NULL)
        return(paste0("magick-image|", key))
    }
    tryCatch(
        paste(class(subview)[[1L]], digest(subview), sep = "|"),
        error = function(e) NULL
    )
}

.subview_grob_memo <- function(converter = as.grob) {
    cache <- new.env(parent = emptyenv())

    function(subview) {
        key <- .subview_cache_key(subview)
        if (!is.null(key) && exists(key, envir = cache, inherits = FALSE)) {
            return(get(key, envir = cache, inherits = FALSE))
        }

        grob <- converter(subview)
        if (!is.null(key)) {
            assign(key, grob, envir = cache)
        }
        grob
    }
}

##' subview geom
##'
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
##' @details
##' Subviews can be placed in two ways.  With a mapping, the positions and the
##' subviews are taken from the data:
##'
##' ```
##' geom_subview(aes(x = x, y = y, subview = pie, width = width, height = width),
##'              data = df)
##' ```
##'
##' For one or more subviews given by position, the first argument is read as the
##' subview rather than as a mapping:
##'
##' ```
##' geom_subview(legend, x = 10, y = 50)
##' geom_subview(df$pie, df$x, df$y, width = df$width, height = df$height)
##' ```
##'
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 aes_
##' @importFrom tibble as_tibble
##' @importFrom tibble tibble
##' @importFrom ggplotify as.grob
##' @importFrom ggfun get_aes_var
## @importFrom grid convertUnit
## @importFrom grid viewport
## @importFrom grid pushViewport
##' @export
##' @author guangchuang yu

geom_subview <- function(mapping = NULL, data = NULL, width=.1, height=.1, x = NULL, y = NULL, subview = NULL) {
    ## can't support `aes(x, y, subview=subview)` as ggplot2 will throw:
    ##     cannot coerce class "c("ggtree", "gg", "ggplot")" to a data.frame

    ## this is a hack to support `aes` without `inherit.aes`. mapping and data must be provided.

    if (!is.null(mapping) && !inherits(mapping, c("uneval", "ggplot2::mapping"))) {
        ## Before the `aes` mapping was supported, the first argument of
        ## geom_subview() was the subview itself:
        ##     geom_subview(subview, x, y, width = , height = )
        ## That form is still the natural one to reach for, and passing a
        ## subview there used to fail with a message that never mentioned the
        ## subview, so keep reading the first argument that way.
        if (!is.null(data) && !is.atomic(data)) {
            stop("the first argument of geom_subview() is `mapping`, but it ",
                 "does not look like an aes() mapping.\n",
                 "  * subviews by position: ",
                 "geom_subview(subview, x, y, width = , height = )\n",
                 "  * with a mapping: ",
                 "geom_subview(aes(x = x, y = y, subview = subview), data = data)",
                 call. = FALSE)
        }
        subview <- mapping
        mapping <- NULL
        if (!is.null(data)) {
            ## the subview was followed by the x and then the y values
            if (!is.null(x)) {
                y <- x
            } else if (is.null(y) && !missing(width)) {
                ## geom_subview(subview, x, y): with nothing named, the third
                ## positional argument lands on `width`, so `y` is there and
                ## `width` goes back to its default (mirrors the signature).
                y <- width
                width <- .1
            }
            x <- data
        }
        data <- NULL
    }

    if (is.null(data)) {
        data <- tibble(x = x, y = y)
    } else if (!inherits(data, 'tbl')) {
        data <- as_tibble(data)
    }

    if (is.null(mapping)) {
        mapping <- aes(x = x, y = y)
    }
    mapping <- as.list(mapping)
    if (is.null(mapping$x)) {
        stop("geom_subview() needs an `x` aesthetic.\n",
             "  * geom_subview(aes(x = x, y = y, subview = subview), data = data)\n",
             "  * geom_subview(subview = subview, x = x, y = y)",
             call. = FALSE)
    }
    if (is.null(mapping$y)) {
        stop("geom_subview() needs a `y` aesthetic.\n",
             "  * geom_subview(aes(x = x, y = y, subview = subview), data = data)\n",
             "  * geom_subview(subview = subview, x = x, y = y)",
             call. = FALSE)
    }
    if (is.null(mapping$subview) && is.null(subview)) {
        stop("geom_subview() needs a subview.\n",
             "  * geom_subview(aes(x = x, y = y, subview = subview), data = data)\n",
             "  * geom_subview(subview = subview, x = x, y = y)",
             call. = FALSE)
    }
    if (is.null(mapping$subview)) {
        if (!inherits(subview, "list")) {
            subview <- list(subview)
        }
        data$subview <- subview
    } else {
        sv_var <- get_aes_var(mapping, 'subview')
        data$subview <- data[[sv_var]]
    }

    xvar <- get_aes_var(mapping, 'x')
    yvar <- get_aes_var(mapping, 'y')

    if (is.null(mapping$width)) {
        data$width <- width
    } else {
        width_var <- get_aes_var(mapping, 'width')
        data$width <- data[[width_var]]
    }

    if (is.null(mapping$height)) {
        data$height <- height
    } else {
        height_var <- get_aes_var(mapping, 'height')
        data$height <- data[[height_var]]
    }


    data$xmin <- data[[xvar]] - data$width/2
    data$xmax <- data[[xvar]] + data$width/2
    data$ymin <- data[[yvar]] - data$height/2
    data$ymax <- data[[yvar]] + data$height/2

    grob_for_subview <- .subview_grob_memo()
    lapply(1:nrow(data), function(i) {
        annotation_custom(
            grob_for_subview(data$subview[[i]]),
            xmin = data$xmin[i],
            xmax = data$xmax[i],
            ymin = data$ymin[i],
            ymax = data$ymax[i])
    })
}


unit <- grid::unit

