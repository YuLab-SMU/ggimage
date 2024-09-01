## geom_ggtree_image <- function() {

## }


##' geom layer for visualizing image files
##'
##'
##' @title geom_image
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param nudge_x horizontal adjustment to nudge image
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' \dontrun{
##' library("ggplot2")
##' library("ggimage")
##' set.seed(2017-02-21)
##' d <- data.frame(x = rnorm(10),
##'                 y = rnorm(10),
##'                 image = sample(c("https://www.r-project.org/logo/Rlogo.png",
##'                                 "https://jeroenooms.github.io/images/frink.png"),
##'                               size=10, replace = TRUE)
##'                )
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image))
##' }
##' @author Guangchuang Yu
geom_image <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", inherit.aes = TRUE,
                       na.rm = FALSE, by = "width", nudge_x = 0, ...) {

  by <- match.arg(by, c("width", "height"))

  layer(
    data = data,
    mapping = mapping,
    geom = GeomImage,
    stat = stat,
    position = position,
    show.legend = NA,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      by = by,
      nudge_x = nudge_x,
      ...),
    check.aes = FALSE
  )
}


##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 draw_key_blank
##' @importFrom grid gTree
##' @importFrom grid gList
##' @importFrom grid nullGrob
GeomImage <- ggproto("GeomImage", Geom,
                     setup_data = function(data, params) {
                       if (is.null(data$subset)) return(data)
                       data[which(data$subset),]
                     },

                     default_aes = aes(image=system.file("extdata/Rlogo.png", package="ggimage"),
                                       size=0.05, colour = NULL, angle = 0, opacity=100),

                     draw_panel = function(data, panel_params, coord, by, na.rm = FALSE,
                                           .fun = NULL, height, image_fun = NULL,
                                           hjust = 0.5, nudge_x = 0, nudge_y = 0, asp = 1) {
                       data$x <- data$x + nudge_x
                       data$y <- data$y + nudge_y

                       valid_rows <- !is.na(data$image) & data$image != ""
                       if (sum(!valid_rows) > 0) {
                         warning(sum(!valid_rows), " rows removed due to invalid image paths")
                         data <- data[valid_rows, ]
                       }

                       if (nrow(data) == 0) {
                         warning("No valid data rows after filtering")
                         return(ggname("geom_image", nullGrob()))
                       }

                       coords <- coord$transform(data, panel_params)
                       plot_width <- diff(panel_params$x.range)
                       plot_height <- diff(panel_params$y.range)

                       coords$group <- seq_len(nrow(coords))  # Each row is its own group
                       grobs <- lapply(split(coords, coords$group), function(group_data) {
                         img <- group_data$image
                         prepared_img <- prepare_image(img, group_data$colour, group_data$opacity, group_data$angle, image_fun)

                         if (is.null(prepared_img)) {
                           warning("Failed to prepare image: ", paste(img, collapse=","))
                           return(NULL)
                         }

                         img_info <- image_info(prepared_img)
                         img_asp <- img_info$width / img_info$height

                         # Use size as a scaling factor (default 0.05 = 5% of plot width)
                         base_size <- min(plot_width, plot_height) * group_data$size[1]

                         if (by == "width") {
                           widths <- base_size
                           heights <- base_size / img_asp
                         } else {
                           heights <- base_size
                           widths <- base_size * img_asp
                         }

                         # Convert to npc units
                         widths_npc <- widths / plot_width
                         heights_npc <- heights / plot_height

                         x_pos <- group_data$x
                         y_pos <- group_data$y

                         # Adjust x position based on hjust
                         if (hjust == 0 || hjust == "left") {
                           x_pos <- x_pos + unit(widths_npc/2, "npc")
                         } else if (hjust == 1 || hjust == "right") {
                           x_pos <- x_pos - unit(widths_npc/2, "npc")
                         }

                         grob <- safe_rasterGrob(x = x_pos,
                                                 y = y_pos,
                                                 image = prepared_img,
                                                 width = unit(widths_npc, "npc"),
                                                 height = unit(heights_npc, "npc"),
                                                 just = c(hjust, 0.5),
                                                 interpolate = TRUE)

                         return(grob)
                       })

                       # Filter out NULL grobs
                       valid_grobs <- Filter(function(x) !is.null(x) && inherits(x, "grob"), grobs)

                       if (length(valid_grobs) == 0) {
                         warning("No valid grobs created")
                         return(ggname("geom_image", nullGrob()))
                       }

                       result <- tryCatch({
                         ggname("geom_image", gTree(children = do.call(gList, valid_grobs)))
                       }, error = function(e) {
                         message("Error in creating gTree: ", e$message)
                         ggname("geom_image", nullGrob())
                       })

                       return(result)
                     },

                     non_missing_aes = c("size", "image"),
                     required_aes = c("x", "y"),
                     draw_key = draw_key_image
)

# Add this function to your implementation
print_plot <- function(p) {
  tryCatch({
    print(p)
  }, error = function(e) {
    message("Error while printing plot: ", e$message)
  })
}

##' @importFrom magick image_read
##' @importFrom magick image_read_svg
##' @importFrom magick image_read_pdf
##' @importFrom magick image_transparent
##' @importFrom magick image_rotate
##' @importFrom grid rasterGrob
##' @importFrom grid viewport
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom methods is
##' @importFrom tools file_ext
imageGrob <- function(x, y, size, img, colour, opacity, angle, adj, image_fun, hjust, by, asp=1, default.units='native') {
  if (is.na(img)) {
    return(zeroGrob())
  }

  # Check cache first
  cached_img <- get_set_cached_image(img)
  if (is.null(cached_img)) {
    if (!is(img, "magick-image")) {
      if (tools::file_ext(img) == "svg") {
        cached_img <- image_read_svg(img)
      } else if (tools::file_ext(img) == "pdf") {
        cached_img <- image_read_pdf(img)
      } else {
        cached_img <- image_read(img)
      }
      get_set_cached_image(img, cached_img)
    } else {
      cached_img <- img
    }
  }

  asp <- getAR2(cached_img)/asp

  if (size == Inf) {
    x <- 0.5
    y <- 0.5
    width <- 1
    height <- 1
  } else if (by == "width") {
    width <- size * adj
    height <- size / asp
  } else {
    width <- size * asp * adj
    height <- size
  }

  if (hjust == 0 || hjust == "left") {
    x <- x + width/2
  } else if (hjust == 1 || hjust == "right") {
    x <- x - width/2
  }

  if (!is.null(image_fun)) {
    cached_img <- image_fun(cached_img)
  }

  if (angle != 0) {
    cached_img <- image_rotate(cached_img, angle)
  }

  if (!is.null(colour)) {
    cached_img <- color_image(cached_img, colour, alpha)
  }

  grob <- rasterGrob(
    x = x,
    y = y,
    image = cached_img,
    default.units = default.units,
    height = height,
    width = if(size == Inf) width else NULL
  )

  return(grob)
}

# Internal cache environment
.image_cache <- new.env(parent = emptyenv())

#' Clear the image cache
#'
#' This function clears the internal image cache used by the package.
#' Call this function if you want to free up memory or ensure fresh image loading.
#'
#' @export
clear_image_cache <- function() {
  rm(list = ls(envir = .image_cache), envir = .image_cache)
}

#' Get the size of the image cache
#'
#' This function returns the number of images currently stored in the cache.
#'
#' @return An integer representing the number of cached images.
#' @export
get_image_cache_size <- function() {
  length(ls(envir = .image_cache))
}

#' Internal function to get or set cached image
#'
#' @param key The key (usually file path) for the image
#' @param value The image object to cache (optional)
#' @return The cached image object or NULL if not found
get_set_cached_image <- function(key, value = NULL) {
  if (is.null(key) || key == "") {
    warning("Invalid key provided to get_set_cached_image")
    return(NULL)
  }

  if (is.null(value)) {
    tryCatch({
      if (exists(key, envir = .image_cache, inherits = FALSE)) {
        return(get(key, envir = .image_cache, inherits = FALSE))
      } else {
        return(NULL)
      }
    }, error = function(e) {
      warning("Error accessing cache for key: ", key, ". Error: ", e$message)
      return(NULL)
    })
  } else {
    tryCatch({
      assign(key, value, envir = .image_cache)
      return(value)
    }, error = function(e) {
      warning("Error setting cache for key: ", key, ". Error: ", e$message)
      return(value)
    })
  }
}

#' Safely check if a value is NULL, NA, or an empty string
#'
#' @param x The value to check
#' @return TRUE if the value is NULL, NA, or an empty string, FALSE otherwise
is_null_or_empty <- function(x) {
  is.null(x) ||
    (is.atomic(x) && length(x) == 1 && (is.na(x) || x == ""))
}

#' Prepare image for rendering
#'
#' @param img The image path or magick image object
#' @param colour The colour to apply to the image
#' @param alpha The alpha value for transparency
#' @param angle The rotation angle
#' @param image_fun A function to apply to the image
#' @return A prepared magick image object or NULL if preparation fails
#' Safely check if a value is NULL, NA, an empty string, or logically invalid
#'
#' @param x The value to check
#' @return TRUE if the value is NULL, NA, an empty string, or logically invalid; FALSE otherwise
is_invalid <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (is.atomic(x) && length(x) == 1 && (is.na(x) || x == "" || !is.character(x)))
}

#' Prepare image for rendering with extensive error checking and logging
#'
#' @param img The image path or magick image object
#' @param colour The colour to apply to the image
#' @param alpha The alpha value for transparency
#' @param angle The rotation angle
#' @param image_fun A function to apply to the image
#' @return A prepared magick image object or NULL if preparation fails
prepare_image <- function(img, colour, opacity, angle, image_fun) {
  tryCatch({
    if (is_invalid(img)) {
      warning("Invalid image path or object provided: ", paste(img, collapse=","))
      return(NULL)
    }

    img_key <- if(is.character(img)) img else digest::digest(img)
    cached_img <- get_set_cached_image(img_key)

    if (is.null(cached_img)) {
      if (!is(img, "magick-image")) {
        if (is.character(img)) {
          cached_img <- switch(tools::file_ext(img),
                               "svg" = image_read_svg(img),
                               "pdf" = image_read_pdf(img),
                               image_read(img))
        } else {
          warning("Unexpected img type: ", class(img))
          return(NULL)
        }
        if (!is.null(cached_img)) {
          get_set_cached_image(img_key, cached_img)
        }
      } else {
        cached_img <- img
      }
    }

    if (is.null(cached_img)) {
      warning("Failed to load image")
      return(NULL)
    }

    if (!is.null(image_fun) && is.function(image_fun)) {
      cached_img <- image_fun(cached_img)
    }

    if (!is.null(angle) && !is.na(angle) && angle != 0) {
      cached_img <- image_rotate(cached_img, angle)
    }

    # add text of image
    # if (!is.null(alpha) && !is.na(alpha)) {
    #   cached_img <- magick::image_annotate(cached_img, text, color = "none",
    #                                strokecolor = "none",
    #                                size = 1)
    # }

    # Handle alpha and colour
    if (!is.null(colour) && !is.na(colour)) {
      cached_img <- image_colorize(cached_img, opacity = opacity, color = colour)
    }

    return(cached_img)
  }, error = function(e) {
    warning("Error in prepare_image: ", e$message)
    return(NULL)
  })
}


# Helper function to safely create a rasterGrob
safe_rasterGrob <- function(x, y, image, width, height, just, interpolate) {
  tryCatch({
    rasterGrob(x = x, y = y, image = image, width = width, height = height,
               just = just, interpolate = interpolate)
  }, error = function(e) {
    warning("Error creating rasterGrob: ", e$message)
    NULL
  })
}

# ##' @importFrom grid makeContent
# ##' @importFrom grid convertHeight
# ##' @importFrom grid convertWidth
# ##' @importFrom grid unit
# ##' @method makeContent fixasp_raster
# ##' @export
# makeContent.fixasp_raster <- function(x) {
#     ## reference https://stackoverflow.com/questions/58165226/is-it-possible-to-plot-images-in-a-ggplot2-plot-that-dont-get-distorted-when-y?noredirect=1#comment102713437_58165226
#     ## and https://github.com/GuangchuangYu/ggimage/issues/19#issuecomment-572523516
#     ## Convert from relative units to absolute units
#     children <- x$children
#     for (i in seq_along(children)) {
#         y <- children[[i]]
#         h <- convertHeight(y$height, "cm", valueOnly = TRUE)
#         w <- convertWidth(y$width, "cm", valueOnly = TRUE)
#         ## Decide how the units should be equal
#         ## y$width <- y$height <- unit(sqrt(h*w), "cm")
#
#         y$width <- unit(w, "cm")
#         y$height <- unit(h, "cm")
#         x$children[[i]] <- y
#     }
#     x
# }

##' @importFrom magick image_info
getAR2 <- function(magick_image) {
  info <- image_info(magick_image)
  info$width/info$height
}


compute_just <- getFromNamespace("compute_just", "ggplot2")


## @importFrom EBImage readImage
## @importFrom EBImage channel
## imageGrob2 <- function(x, y, size, img, by, colour, alpha) {
##     if (!is(img, "Image")) {
##         img <- readImage(img)
##         asp <- getAR(img)
##     }

##     unit <- "native"
##     if (any(size == Inf)) {
##         x <- 0.5
##         y <- 0.5
##         width <- 1
##         height <- 1
##         unit <- "npc"
##     } else if (by == "width") {
##         width <- size
##         height <- size/asp
##     } else {
##         width <- size * asp
##         height <- size
##     }

##     if (!is.null(colour)) {
##         color <- col2rgb(colour) / 255

##         img <- channel(img, 'rgb')
##         img[,,1] <- colour[1]
##         img[,,2] <- colour[2]
##         img[,,3] <- colour[3]
##     }

##     if (dim(img)[3] >= 4) {
##         img[,,4] <- img[,,4]*alpha
##     }

##     rasterGrob(x = x,
##                y = y,
##                image = img,
##                default.units = unit,
##                height = height,
##                width = width,
##                interpolate = FALSE)
## }


## getAR <- function(img) {
##     dims <- dim(img)[1:2]
##     dims[1]/dims[2]
## }


##################################################
##                                              ##
## another solution, but the speed is too slow  ##
##                                              ##
##################################################

## draw_key_image <- function(data, params, size) {
##     imageGrob(0.5, 0.5, image=data$image, size=data$size)
## }


## ##' @importFrom ggplot2 ggproto
## ##' @importFrom ggplot2 Geom
## ##' @importFrom ggplot2 aes
## ##' @importFrom ggplot2 draw_key_blank
## GeomImage <- ggproto("GeomImage", Geom,
##                      non_missing_aes = c("size", "image"),
##                      required_aes = c("x", "y"),
##                      default_aes = aes(size=0.05, image="https://www.r-project.org/logo/Rlogo.png"),
##                      draw_panel = function(data, panel_scales, coord, by, na.rm=FALSE) {
##                          data$image <- as.character(data$image)
##                          data <- coord$transform(data, panel_scales)
##                          imageGrob(data$x, data$y, data$image, data$size, by)
##                      },
##                      draw_key = draw_key_image
##                      )


## ##' @importFrom grid grob
## imageGrob <- function(x, y, image, size=0.05, by="width") {
##     grob(x=x, y=y, image=image, size=size, by=by, cl="image")
## }

## ##' @importFrom grid drawDetails
## ##' @importFrom grid grid.raster
## ##' @importFrom EBImage readImage
## ##' @method drawDetails image
## ##' @export
## drawDetails.image <- function(x, recording=FALSE) {
##     image_object <- lapply(x$image, readImage)
##     names(image_object) <- x$image
##     for (i in seq_along(x$image)) {
##         img <- image_object[[x$image[i]]]
##         size <- x$size[i]
##         by <- x$by
##         asp <- getAR(img)
##         if (is.na(size)) {
##             width <- NULL
##             height <- NULL
##         } else if (by == "width") {
##             width <- size
##             height <- size/asp
##         } else {
##             width <- size * asp
##             height <- size
##         }

##         grid.raster(x$x[i], x$y[i],
##                     width = width,
##                     height = height,
##                     image = img,
##                     interpolate=FALSE)
##     }
## }

## ##' @importFrom ggplot2 discrete_scale
## ##' @importFrom scales identity_pal
## ##' @importFrom ggplot2 ScaleDiscreteIdentity
## ##' @export
## scale_image <- function(..., guide = "legend") {
##   sc <- discrete_scale("image", "identity", identity_pal(), ..., guide = guide,
##                        super = ScaleDiscreteIdentity)

##   sc
## }
