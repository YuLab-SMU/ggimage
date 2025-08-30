#' @title Create interactive image of ggimage 
#' @description
#' The geometry is based on `geom_image()`.
#' See the documentation for those functions for more details.
#' @param ... see also the parameters of `geom_image()` of `ggimage`
#' @export
geom_image_interactive <- function(...){
  # rlang::check_installed(c('ggiraph'), "for `geom_image_interactive()`.")
  layer_interactive(geom_image, interactive_geom = GeomInteractiveImage,...)
}


#' @title Create interactive phylopic of ggimage
#' @description
#' The geometry is based on `geom_phylopic()`.
#' See the documentation for those functions for more details.
#' @param ... see also the parameters of `geom_phylopic()` of `ggimage`
#' @export
geom_phylopic_interactive <- function(...){
  # rlang::check_installed(c('ggiraph'), "for `geom_phylopic_interactive()`.")
  layer_interactive(geom_phylopic, interactive_geom = GeomInteractiveImage,...)
}

# the internal functions of ggiraph

#' @importFrom purrr detect_index
#' @importFrom rlang caller_env
#' @import ggiraph
layer_interactive <- getFromNamespace("layer_interactive", "ggiraph")
add_default_interactive_aes <- getFromNamespace("add_default_interactive_aes", "ggiraph")
interactive_geom_parameters <- getFromNamespace("interactive_geom_parameters", "ggiraph")
interactive_geom_draw_key <- getFromNamespace("interactive_geom_draw_key", "ggiraph")
IPAR_NAMES <- getFromNamespace("IPAR_NAMES", "ggiraph")
add_interactive_attrs <- getFromNamespace("add_interactive_attrs", "ggiraph")


#' @title ggproto classes for ggiraph
#' @description
#' ggproto classes for ggiraph
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
GeomInteractiveImage <- ggproto(
  "GeomInteractiveImage",
  GeomImage,
  default_aes = add_default_interactive_aes(GeomImage),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(data, 
                        panel_params, 
                        coord,
                        by,
                        na.rm = FALSE, 
                        .fun = NULL,
                        image_fun = NULL, 
                        hjust = .5,
                        nudge_x = 0, 
                        nudge_y = 0,
                        asp = 1,
                        use_cache = TRUE,
                        .ipar = IPAR_NAMES
                        ){
    data <- GeomImage$make_image_data(data, panel_params, coord, .fun, nudge_x, nudge_y) 
    adjs <- GeomImage$build_adjust(data, panel_params, by)
    grobs <- lapply(seq_len(nrow(data)), function(i){
         gb <- imageGrob(x = data$x[i],
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
         gb <- add_interactive_attrs(gb, data[i,], ipar=.ipar)
         return(gb)
        })
    ggname("geom_image_interactive", gTree(children = do.call(gList, grobs)))    
  }
)

