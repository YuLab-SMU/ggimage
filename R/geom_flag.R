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

## Build the flag URL without validating it.  Kept separate from flag() so that
## callers which build many URLs at once (worldcup2018()) do not pay for one
## request per flag.
flag_url <- function(flag) {
    ## no long works, see issue #55
    ## this repo now only contains svg files
    # url <- paste0('https://raw.githubusercontent.com/hampusborgos/country-flags/main/png250px/', tolower(flag), ".png")

    sprintf("https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/%s.png", toupper(flag))
}

flag <- function(flag) {
    check_url(flag_url(flag))
}


##' list available flag
##'
##'
##' @title list.flag
##' @return flag vector
##' @export
##' @author Guangchuang Yu
list.flag <- function() {
  # https://github.com/fonttools/region-flags/tree/gh-pages/png
  files <- get_github_files("fonttools", "region-flags", "png", "gh-pages")
  sub(".png", "", files)
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
    ## The flags used to be 32 hard-coded URLs on upload.wikimedia.org, which is
    ## not reachable from every network (it times out from mainland China, so
    ## geom_worldcup2018() was unusable there).  Build them from the same host as
    ## flag() instead, which is the one geom_flag() already relies on.
    codes <- list.worldcup2018()
    code <- toupper(country)
    code[!code %in% codes] <- NA_character_
    ## England has no ISO 3166-1 code of its own.
    code[code %in% "EN"] <- "GB-ENG"

    flag <- rep(NA_character_, length(code))
    known <- !is.na(code)
    flag[known] <- flag_url(code[known])
    names(flag) <- toupper(country)
    return(flag)
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

