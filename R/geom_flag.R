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

## Build the flag URL without validating the host, only the code.  Kept
## separate from flag() so that callers which build many URLs at once
## (worldcup2018()) do not pay for one request per flag.
## An NA or empty code would otherwise be upper-cased into the literal "NA"
## and resolve to Namibia's flag (the repo really has NA.png), so a missing
## code yields NA, which the drawing layer skips -- the contract every other
## image geom follows.  Codes with characters that cannot be a country or
## subdivision code (spaces, slashes, "..") could never name a file either and
## are dropped the same way.
flag_url <- function(flag) {
    ## no long works, see issue #55
    ## this repo now only contains svg files
    # url <- paste0('https://raw.githubusercontent.com/hampusborgos/country-flags/main/png250px/', tolower(flag), ".png")

    code <- toupper(flag)
    valid <- !is.na(code) & grepl("^[A-Z]{2}(-[A-Z0-9]{1,3})?$", code)
    url <- rep(NA_character_, length(code))
    url[valid] <- sprintf("https://raw.githubusercontent.com/fonttools/region-flags/refs/heads/gh-pages/png/%s.png", code[valid])
    url
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
    ## A dropped code is worth mentioning, but `NA` is not one: a missing row is
    ## legitimate data here and every other geom treats it as "draw nothing".
    unknown <- !is.na(code) & !code %in% codes
    dropped <- if (any(unknown)) unique(code[unknown]) else character()
    code[unknown] <- NA_character_
    if (length(dropped)) {
        warning("geom_worldcup2018() draws nothing for ", length(dropped),
                " code(s) outside the 32 qualifiers: ",
                paste(dropped, collapse = ", "), call. = FALSE)
    }
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

