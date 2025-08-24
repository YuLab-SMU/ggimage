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
geom_image <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", inherit.aes=TRUE,
                       na.rm=FALSE, by="width", nudge_x = 0, ...) {

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
                                           hjust=0.5, nudge_x = 0, nudge_y = 0, asp=1) {
                         data <- GeomImage$make_image_data(data, panel_params, coord, .fun, nudge_x, nudge_y)

                         adjs <- GeomImage$build_adjust(data, panel_params, by)

                         grobs <- lapply(seq_len(nrow(data)), function(i){
                              imageGrob(x = data$x[i], 
                                        y = data$y[i], 
                                        size = data$size[i], 
                                        img = data$image[i],
                                        colour = data$colour[i],
                                        alpha = data$alpha[i],
                                        angle = data$angle[i],
                                        adj = adjs[i],
                                        image_fun = image_fun,
                                        hjust = hjust,
                                        by = by,
                                        asp = asp
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
                     draw_key = draw_key_image ## draw_key_blank ## need to write the `draw_key_image` function.
                     )


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
imageGrob <- function(x, y, size, img, colour, alpha, angle, adj, image_fun, hjust, by, asp=1, default.units='native'){
    if (is.na(img)){
        return(zeroGrob())
    }
    if (!is(img, "magick-image")) {
        if (tools::file_ext(img) == "svg") {
            img <- image_read_svg(img)
        } else if (tools::file_ext(img) == "pdf") {
            img <- image_read_pdf(img)
        } else {
            img <- image_read(img)
        }
        asp <- getAR2(img)/asp
    }

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
        img <- image_fun(img)
    }    

    if (angle != 0) {
        img <- image_rotate(img, angle)
    }

    if (!is.null(colour)){
        img <- color_image(img, colour, alpha)
    }
    
    if (size == Inf){
       grob <- rasterGrob(x = x,
                          y = y,
                          image = img,
                          default.units = default.units,
                          height = height,
                          width = width
                          )
    }else{
       grob <- rasterGrob(
                          x = x,
                          y = y,
                          image = img,
                          default.units = default.units,
                          height = height
                       )        
    }
    return(grob)
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
