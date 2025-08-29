##' geom layer for using icon
##'
##'
##' @title geom_icon
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_icon <- function(mapping = NULL, data = NULL, inherit.aes = TRUE,
                      na.rm = FALSE, by = "width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = icon)
}

icon <- function(id) {
    ## paste0('https://raw.githubusercontent.com/ionic-team/ionicons/main/src/svg/', id, '.svg')
    url <- paste0('https://ionicons.com/ionicons/svg/', id, '.svg')

    check_url(url)
}

##' list available icon
##'
##'
##' @title list.icon
##' @return icon vector
##' @export
##' @author Guangchuang Yu
list.icon <- function() {
    # list.png("https://github.com/ionic-team/ionicons/tree/main/src/svg")
    files <- get_github_files("ionic-team", "ionicons", "src/svg", "main")
    sub(".svg", "", files)
}
