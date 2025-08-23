##' geom layer for using flag image
##'
##'
##' @title geom_flag
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_flag <- function(mapping=NULL, 
                    data=NULL, 
                    inherit.aes=TRUE,
                    na.rm=FALSE, 
                    by="width", 
                    ...) {
  
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = flag)
}

flag <- function(flag) {
    ## no long works, see issue #55
    ## this repo now only contains svg files
    # url <- paste0('https://raw.githubusercontent.com/hampusborgos/country-flags/main/png250px/', tolower(flag), ".png")

    url <- sprintf("https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/%s.png", toupper(flag))

    check_url(url)
}


##' list available flag
##'
##'
##' @title list.flag
##' @return flag vector
##' @export
##' @author Guangchuang Yu
list.flag <- function() {
     x <- readLines("https://github.com/behdad/region-flags/tree/gh-pages/png")
     y <- x[grep("title=\"\\w+\\.png", x)]
     sub(".*title=\"(\\w+)\\.png.*", '\\1', y)
}


##' list flags of worldcup 2018
##'
##' @title list.worldcup2018
##' @return flag vector
##' @export
##' @author Guangchuang Yu
list.worldcup2018 <- function() {
    cc <- c("ar", "au", "be", "br", "co", "cr", "hr", "dk",
            "eg", "en", "fr", "de", "is", "ir", "jp", "kr",
            "mx", "ma", "ng", "pa", "pe", "pl", "pt", "ru",
            "sa", "sn", "rs", "es", "se", "ch", "tn", "uy")
    return(toupper(cc))
}

worldcup2018 <- function(country) {
    ## xx <- sapply(cc, function(x) jsonlite::fromJSON(paste0("https://kiwi-check.glitch.me/country_data?cc=", x))$flag_png)
    ## y <- paste(xx, collapse='",\n"', sep="")
    ## cat(y)
    
    flag <- c(
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Flag_of_Argentina.svg/320px-Flag_of_Argentina.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Flag_of_Australia_%28converted%29.svg/320px-Flag_of_Australia_%28converted%29.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Belgium_%28civil%29.svg/320px-Flag_of_Belgium_%28civil%29.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/0/05/Flag_of_Brazil.svg/320px-Flag_of_Brazil.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Flag_of_Colombia.svg/320px-Flag_of_Colombia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/Flag_of_Costa_Rica.svg/320px-Flag_of_Costa_Rica.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Flag_of_Croatia.svg/320px-Flag_of_Croatia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Denmark.svg/317px-Flag_of_Denmark.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Egypt.svg/320px-Flag_of_Egypt.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/b/be/Flag_of_England.svg/800px-Flag_of_England.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/c/c3/Flag_of_France.svg/320px-Flag_of_France.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/320px-Flag_of_Germany.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Iceland.svg/320px-Flag_of_Iceland.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/Flag_of_Iran.svg/320px-Flag_of_Iran.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Flag_of_Japan.svg/320px-Flag_of_Japan.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Flag_of_South_Korea.svg/320px-Flag_of_South_Korea.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Flag_of_Mexico.svg/200px-Flag_of_Mexico.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2c/Flag_of_Morocco.svg/320px-Flag_of_Morocco.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Flag_of_Nigeria.svg/320px-Flag_of_Nigeria.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Flag_of_Panama.svg/200px-Flag_of_Panama.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/Flag_of_Peru.svg/320px-Flag_of_Peru.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Flag_of_Poland.svg/320px-Flag_of_Poland.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Portugal.svg/320px-Flag_of_Portugal.svg.png",
        "https://upload.wikimedia.org/wikipedia/en/thumb/f/f3/Flag_of_Russia.svg/320px-Flag_of_Russia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/Flag_of_Saudi_Arabia.svg/320px-Flag_of_Saudi_Arabia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fd/Flag_of_Senegal.svg/320px-Flag_of_Senegal.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Flag_of_Serbia.svg/320px-Flag_of_Serbia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Spain.svg/320px-Flag_of_Spain.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Flag_of_Sweden.svg/320px-Flag_of_Sweden.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Flag_of_Switzerland.svg/240px-Flag_of_Switzerland.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Tunisia.svg/320px-Flag_of_Tunisia.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Uruguay.svg/320px-Flag_of_Uruguay.svg.png"
    )
    names(flag) <- list.worldcup2018()
    return(flag[country])
}

##' geom layer for wordcup 2018
##'
##' @title geom_worldcup2018
##' @inheritParams geom_flag
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_worldcup2018 <-  function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                               na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = worldcup2018)
}

