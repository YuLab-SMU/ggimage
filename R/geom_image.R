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
##' @param use_cache logical, whether to use image caching for better performance (default: TRUE)
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
##' # With caching enabled (default)
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image))
##'
##' # With caching disabled
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image), use_cache=FALSE)
##' }
##' @author Guangchuang Yu
geom_image <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", inherit.aes=TRUE,
                       na.rm=FALSE, by="width", nudge_x = 0, use_cache=TRUE, ...) {

    by <- match.arg(by, c("width", "height"))

    layer(
        data=data,
        mapping=mapping,
        geom=GeomImage,
        stat=stat,
        position=position,
        show.legend=NA,
        inherit.aes=inherit.aes,
        params = list(
            na.rm = na.rm,
            by = by,
            nudge_x = nudge_x,
            use_cache = use_cache,
            ##angle = angle,
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
GeomImage <- ggproto("GeomImage", Geom,
                     setup_data = function(data, params) {
                         if (is.null(data$subset))
                             return(data)
                         data[which(data$subset),]
                     },

                     default_aes = aes(image=system.file("extdata/Rlogo.png", package="ggimage"),
                                       size=0.05, colour = NULL, angle = 0, alpha=1),

                     draw_panel = function(data, panel_params, coord, by, na.rm=FALSE,
                                           .fun = NULL, image_fun = NULL,
                                           hjust=0.5, nudge_x = 0, nudge_y = 0, asp=1, use_cache=TRUE) {
                         data <- GeomImage$make_image_data(data, panel_params, coord, .fun, nudge_x, nudge_y)

                         adjs <- GeomImage$build_adjust(data, panel_params, by)

                         grobs <- lapply(seq_len(nrow(data)), function(i){
                              imageGrob(x = data$x[i],
                                        y = data$y[i],
                                        size = data$size[i],
                                        img = data$image[i],
                                        colour = data$colour[i],
                                        opacity = data$alpha[i],
                                        angle = data$angle[i],
                                        adj = adjs[i],
                                        image_fun = image_fun,
                                        hjust = hjust,
                                        by = by,
                                        asp = asp,
                                        use_cache = use_cache
                              )
                             })
                         ggname("geom_image", gTree(children = do.call(gList, grobs)))
                     },
                     make_image_data = function(data, panel_params, coord, .fun, nudge_x = 0, nudge_y = 0,...){
                         data$x <- data$x + nudge_x
                         data$y <- data$y + nudge_y
                         data <- coord$transform(data, panel_params)

                         if (!is.null(.fun) && is.function(.fun)) {
                             data$image <- .fun(data$image)
                         }
                         if (is.null(data$image)){
                             return(NULL)
                         }else{
                             return(data)
                         }
                     },
                     build_adjust = function(data, panel_params, by){
                         if (by=='height' && "y.range" %in% names(panel_params)) {
                             adjs <- data$size / diff(panel_params$y.range)
                         } else if (by == 'width' && "x.range" %in% names(panel_params)){
                             adjs <- data$size / diff(panel_params$x.range)
                         } else if ("r.range" %in% names(panel_params)) {
                             adjs <- data$size / diff(panel_params$r.range)
                         } else {
                             adjs <- data$size
                         }
                         adjs[is.infinite(adjs)] <- 1
                         return(adjs)
                     },
                     non_missing_aes = c("size", "image"),
                     required_aes = c("x", "y"),
                     draw_key = draw_key_blank ## draw_key_blank ## need to write the `draw_key_image` function.
                     )

#### caching mechanism for images ####
# Using yulab.utils cache system:
# - get_cache_item() auto-initializes cache items if they don't exist
# - get_cache_element() retrieves specific elements from cache items
# - update_cache_item() stores data in cache items
# - rm_cache_item() removes cache items
.IMAGE_CACHE_ITEM <- ".ggimage_cache_image"
.IMAGE_TRANSFORM_CACHE_ITEM <- ".ggimage_cache_image_transform"

# Helper function to check if image is invalid
is_invalid <- function(img) {
  is.null(img) || length(img) == 0 || (is.character(img) && (is.na(img) || img == ""))
}

# generate stable key：path→standardized character；object→digest
#' @importFrom digest digest
image_cache_key <- function(img) {
  if (is.character(img)) {
    # standardized path（not force mustWork，allow remote URL or path don't exits）
    kp <- tryCatch(normalizePath(img, winslash = "/", mustWork = FALSE),
                   error = function(e) img)
    return(kp)
  }
  # for non-character：use digest
  digest(img)
}

# generate transform key based on base_key and transform parameters
image_transform_key <- function(base_key, angle, colour, opacity) {
  paste(base_key,
        if (is.null(angle) || is.na(angle)) 0 else angle,
        if (is.null(colour) || is.na(colour)) "" else as.character(colour),
        if (is.null(opacity) || is.na(opacity)) "" else as.character(opacity),
        sep = "|")
}

# store image into cache
cache_get_image <- function(key, use_cache = TRUE) {
  if (!use_cache) return(NULL)
  # attempt to read from cache; yulab.utils handles initialization
  tryCatch(get_cache_element(.IMAGE_CACHE_ITEM, key), error = function(e) NULL)
}

# write cache (base image)
cache_set_image <- function(key, value, use_cache = TRUE) {
  if (!use_cache) return(invisible(value))
  tryCatch(update_cache_item(.IMAGE_CACHE_ITEM, stats::setNames(list(value), key)),
           error = function(e) invisible(NULL))
  invisible(value)
}

# read cache（transformed image）
cache_get_transformed <- function(tkey, use_cache = TRUE) {
  if (!use_cache) return(NULL)
  tryCatch(get_cache_element(.IMAGE_TRANSFORM_CACHE_ITEM, tkey), error = function(e) NULL)
}

# write cache（after transformed）
cache_set_transformed <- function(tkey, value, use_cache = TRUE) {
  if (!use_cache) return(invisible(value))
  tryCatch(update_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM, stats::setNames(list(value), tkey)),
           error = function(e) invisible(NULL))
  invisible(value)
}

# clean all image cache（Optional）
clear_image_cache <- function() {
  tryCatch(rm_cache_item(.IMAGE_CACHE_ITEM), error = function(e) invisible(NULL))
  tryCatch(rm_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM), error = function(e) invisible(NULL))
}

# Stat cache（Optional，return length）
get_image_cache_size <- function() {
  ci <- tryCatch(get_cache_item(.IMAGE_CACHE_ITEM), error = function(e) NULL)
  length(ci)
}

get_image_transform_cache_size <- function() {
  ci <- tryCatch(get_cache_item(.IMAGE_TRANSFORM_CACHE_ITEM), error = function(e) NULL)
  length(ci)
}

prepare_image <- function(img, colour, opacity, angle, image_fun, use_cache = TRUE) {
  tryCatch({
    if (is_invalid(img)) {
      warning("Invalid image path or object provided: ", paste(img, collapse=","))
      return(NULL)
    }

    # Unified generation of base images key
    img_key <- image_cache_key(img)

    # First check the base images cache
    cached_img <- cache_get_image(img_key, use_cache)

    # if don't hit the cache, read the raw image file.
    if (is.null(cached_img)) {
      if (!methods::is(img, "magick-image")) {
        if (is.character(img)) {
          cached_img <- switch(tools::file_ext(img),
                               "svg" = magick::image_read_svg(img),
                               "pdf" = magick::image_read_pdf(img),
                               magick::image_read(img))
        } else {
          warning("Unexpected img type: ", class(img))
          return(NULL)
        }
      } else {
        cached_img <- img
      }
      if (!is.null(cached_img)) {
        cache_set_image(img_key, cached_img, use_cache)
      }
    }

    if (is.null(cached_img)) {
      warning("Failed to load image")
      return(NULL)
    }

    # —— secondary cache（optional）：cache for angle/colour/opacity image —— #
    tkey <- image_transform_key(img_key, angle, colour, opacity)
    transformed <- cache_get_transformed(tkey, use_cache)

    if (is.null(transformed)) {
      # apply the available user function
      if (!is.null(image_fun) && is.function(image_fun)) {
        transformed <- image_fun(cached_img)
      } else {
        transformed <- cached_img
      }

      # rotate
      if (!is.null(angle) && !is.na(angle) && angle != 0) {
        transformed <- magick::image_rotate(transformed, angle)
      }
      # color/opacity（execute for the color setting）
      if (!is.null(colour) && !is.na(colour)) {
        # ?? image_colorize(opacity=?, color=?)
        # use the same API；opacity set as 0-100 percentage
        transformed <- magick::image_colorize(transformed, opacity = 100, color = colour)
        if (!is.null(opacity) && !is.na(opacity) && unique(opacity) != 1){
            transformed <- magick::image_fx(transformed, expression = paste0("u.a * ", opacity), channel = "alpha")
        }
        #transformed <- color_image(transformed, colour, opacity)
      }

      cache_set_transformed(tkey, transformed, use_cache)
    }

    transformed
  }, error = function(e) {
    warning("Error in prepare_image: ", e$message)
    NULL
  })
}

#### caching mechanism for images end ####

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
##' @importFrom yulab.utils get_cache_element update_cache_item rm_cache_item get_cache_item
imageGrob <- function(x, y, size, img, colour, opacity, angle, adj, image_fun, hjust, by, asp=1, default.units='native', use_cache=TRUE) {
  if (is.na(img)) {
    return(zeroGrob())
  }

  # Use prepare_image for unified caching and transformation
  cached_img <- prepare_image(img, colour, opacity, angle, image_fun, use_cache)

  if (is.null(cached_img)) {
    return(zeroGrob())
  }

  asp <- getAR2(cached_img)/asp

  if (size == Inf) {
    x <- 0.5; y <- 0.5; width <- 1; height <- 1
  } else if (by == "width") {
    width <- size * adj; height <- size / asp
  } else {
    width <- size * asp * adj; height <- size
  }

  if (hjust == 0 || hjust == "left") {
    x <- x + width/2
  } else if (hjust == 1 || hjust == "right") {
    x <- x - width/2
  }

  grob <- rasterGrob(
    x = x, y = y, image = cached_img, default.units = default.units,
    height = height, width = if (size == Inf) width else NULL
  )
  grob
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
