##' geom layer for using emoji image
##'
##'
##' @title geom_emoji
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_emoji <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = emoji)
}

emoji <- function(emoji) {
    ## twemoji.maxcdn.com was decommissioned: it still answers 200 but with a
    ## 35-byte text file, so the download is not a valid PNG and the layer dies
    ## with "improper image header" while the grob is being built.
    url <- paste0('https://raw.githubusercontent.com/jdecked/twemoji/main/assets/72x72/',
                  emoji, ".png")

    check_url(url)
}

