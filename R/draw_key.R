

##' key drawing function
##'
##' `draw_key_image()` draws the legend key of image layers (i.e. `geom_image()`).
##' The type of the key is determined by the global option `ggimage.keytype`,
##' which supports `"point"` (the default when the option is unset), `"rect"`,
##' `"image"` and `"blank"` (no key, the behaviour of image layers before the
##' key was implemented). An unsupported value falls back to `"point"` with a
##' warning.
##'
##' The `"image"` key shows the image carried by the key data, which is the
##' layer's `image` argument or the images a guide is given with `override.aes`;
##' ggplot2 has no scale with which to look a per-row `image` up, so a layer that
##' maps `image` to a column puts the same image in every key. Without any image
##' to show, the key falls back to the logo bundled with the package.
##'
##' Image layers have no default colour, so `colour` can be missing (i.e. `NULL`)
##' or `NA` in `data`, for instance when only `alpha` is mapped. Such keys fall
##' back to `"black"`, so that `alpha` stays visible in the legend.
##'
##' @name draw_key
##' @param data A single row data frame containing the scaled aesthetics to display in this key
##' @param params A list of additional parameters supplied to the geom.
##' @param size Width and height of key in mm
##' @return A grid grob
NULL


ggname <- getFromNamespace("ggname", "ggplot2")

## image layers have no default colour; `colour` is NULL when it is not mapped
## and `alpha` can be NA, both need a value that keeps the key visible.
key_colour <- function(colour, n = 1L, default = "black") {
    if (is.null(colour) || length(colour) == 0L) {
        colour <- default
    }
    if (length(colour) != n) {
        colour <- rep(colour, length.out = n)
    }
    colour[is.na(colour)] <- default
    colour
}

key_alpha <- function(alpha, n = 1L) {
    if (is.null(alpha) || length(alpha) == 0L) {
        alpha <- 1
    }
    if (length(alpha) != n) {
        alpha <- rep(alpha, length.out = n)
    }
    alpha[is.na(alpha)] <- 1
    alpha
}

keytype <- function(kt) {
    supported <- c("point", "rect", "image", "blank")
    if (!(is.character(kt) && length(kt) == 1L && !is.na(kt) && kt %in% supported)) {
        warning("Unsupported `ggimage.keytype` option: ",
                paste(format(kt), collapse = ", "), ". Using \"point\".")
        kt <- "point"
    }
    kt
}

## Keep the bundled logo in the package namespace for the lifetime of the
## session.  This is deliberately separate from the user image cache: the
## legend image has a fixed package-local path and must not be persisted to
## disk or survive unloading/reloading the package.
.ggimage_key_image_cache <- new.env(parent = emptyenv())

load_key_image <- function() {
    image_read(system.file("extdata/Rlogo.png", package = "ggimage"))
}

get_key_image <- function() {
    cache <- .ggimage_key_image_cache
    if (!exists("image", envir = cache, inherits = FALSE)) {
        assign("image", load_key_image(), envir = cache)
    }
    get("image", envir = cache, inherits = FALSE)
}

## One key per group, so a legend can tell categories apart by their pictures.
## Layers that map no image, and keys requested by hand without one, keep the
## bundled logo.
key_images <- function(data, n) {
    values <- data$image
    if (is.null(values) || length(values) == 0L) {
        return(rep(list(get_key_image()), n))
    }
    if (is.list(values)) {
        return(values[rep(seq_along(values), length.out = n)])
    }
    if (is.factor(values)) {
        values <- as.character(values)
    }
    lapply(seq_len(n), function(i) values[[(i - 1L) %% length(values) + 1L]])
}

##' @rdname draw_key
##' @importFrom grid rectGrob
##' @importFrom grid pointsGrob
##' @importFrom grid rasterGrob
##' @importFrom grid gpar
##' @importFrom scales alpha
##' @export
draw_key_image <- function(data, params, size) {
    kt <- getOption("ggimage.keytype")
    if (is.null(kt)) {
        kt <- "point"
    }
    kt <- keytype(kt)

    if (kt == "blank") {
        return(zeroGrob())
    }

    ## ggplot2 draws one key at a time, but `data` may hold several rows when
    ## the key is requested by hand (e.g. `data` of all groups of a layer).
    n <- max(1L, nrow(data))
    opacity <- key_alpha(data$alpha, n)

    if (kt == "image") {
        img_list <- key_images(data, n)
        ## no colour to colorize with, the image is displayed as is
        colour <- key_colour(data$colour, n, default = NA_character_)
        grobs <- lapply(seq_len(n), function(i) {
            key_img <- prepare_image(
                img_list[[i]],
                colour = colour[i],
                opacity = opacity[i],
                angle = 0,
                image_fun = params$image_fun,
                use_cache = TRUE
            )
            if (is.null(key_img)) {
                return(zeroGrob())
            }

            rasterGrob(
                0.5, 0.5,
                image = key_img,
                width = 1,
                height = 1
            )
        })
    } else {
        colour <- key_colour(data$colour, n, "black")
        if (kt == "point") {
            grobs <- lapply(seq_len(n), function(i) {
                pointsGrob(
                    0.5, 0.5,
                    pch = 19,
                    gp = gpar (
                        col = alpha(colour[i], opacity[i]),
                        fill = alpha(colour[i], opacity[i]),
                        fontsize = 3 * ggplot2::.pt,
                        lwd = 0.94
                        )
                )
            })
        } else { ## kt == "rect"
            grobs <- lapply(seq_len(n), function(i) {
                rectGrob(gp = gpar(
                             col = NA,
                             fill = alpha(colour[i], opacity[i])
                             ))
            })
        }
    }

    ## a single key keeps the plain grob, several keys are wrapped in a gTree
    if (kt != "image" && length(grobs) == 1L) {
        return(grobs[[1]])
    }

    class(grobs) <- "gList"

    keyGrob <- ggname("image_key",
                      gTree(children = grobs))
    return(keyGrob)
}


